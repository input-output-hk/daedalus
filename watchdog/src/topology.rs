//! Name resolution for cardano-node's topology.
//!
//! On Windows, a DNS lookup made by cardano-node can wait for a reply that
//! never comes, and while it does the node does not stop when asked to
//! (IntersectMBO/ouroboros-network#5448). With `node.resolve_topology` the
//! watchdog resolves the names in the topology file itself before each node
//! start, with its own time limit, and gives the node a copy that holds
//! addresses for every name that resolves now or has resolved before.
//!
//! The lookup uses the operating system resolver, on a thread of its own: a
//! lookup that never returns is abandoned after the limit, and neither holds
//! up the start nor the watchdog's exit.

use std::collections::{BTreeMap, BTreeSet};
use std::io;
use std::net::{IpAddr, ToSocketAddrs};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::io::AsyncWriteExt;
use tokio::time::timeout;
use tracing::{info, warn};

use crate::config::NodeConfig;
use crate::state;
use crate::supervisor::unix_ms;

/// The topology the node is given, written next to the watchdog's state.
const RESOLVED_TOPOLOGY_FILE: &str = "topology-resolved.json";
/// The addresses each name last resolved to.
const CACHE_FILE: &str = "resolved-relays.json";

/// What a lookup of one name came to.
#[derive(Debug, Clone)]
pub struct Lookup {
    /// The addresses in preference order (`prefer_ipv4`), or why there are
    /// none.
    pub result: Result<Vec<IpAddr>, String>,
    pub took: Duration,
}

/// Resolve `host` with the operating system resolver. The call runs on its own
/// thread, which is left to finish if the caller stops waiting.
pub async fn os_lookup(host: String) -> io::Result<Vec<IpAddr>> {
    let (tx, rx) = tokio::sync::oneshot::channel();
    std::thread::Builder::new()
        .name("dns-lookup".to_string())
        .spawn(move || {
            let result = (host.as_str(), 0u16)
                .to_socket_addrs()
                .map(|found| found.map(|a| a.ip()).collect::<Vec<_>>());
            let _ = tx.send(result);
        })?;
    rx.await
        .unwrap_or_else(|_| Err(io::Error::other("the lookup thread ended without a result")))
}

/// IPv4 addresses when there are any, IPv6 addresses otherwise, each address
/// once and in the order given. Unspecified addresses (`0.0.0.0`, `::`), which
/// a filtering resolver answers for a blocked name, are dropped: the node
/// refuses a topology that holds one. On a machine without an IPv6 route, a
/// connection to an IPv6 address fails, so IPv4 addresses are used whenever a
/// name has any.
pub fn prefer_ipv4(addresses: &[IpAddr]) -> Vec<IpAddr> {
    let mut unique: Vec<IpAddr> = Vec::new();
    for a in addresses {
        if !a.is_unspecified() && !unique.contains(a) {
            unique.push(*a);
        }
    }
    if unique.iter().any(IpAddr::is_ipv4) {
        unique.retain(IpAddr::is_ipv4);
    }
    unique
}

/// Resolve every name in parallel, each within `limit`. A name that errs, has
/// no address or does not answer in time has an `Err` result.
pub async fn resolve_names<F, Fut>(
    names: &BTreeSet<String>,
    limit: Duration,
    lookup: F,
) -> BTreeMap<String, Lookup>
where
    F: Fn(String) -> Fut,
    Fut: std::future::Future<Output = io::Result<Vec<IpAddr>>> + Send + 'static,
{
    let mut tasks = tokio::task::JoinSet::new();
    for name in names {
        let name = name.clone();
        let pending = lookup(name.clone());
        tasks.spawn(async move {
            let started = Instant::now();
            let result = match timeout(limit, pending).await {
                Err(_) => Err(format!("no answer within {} ms", limit.as_millis())),
                Ok(Err(e)) => Err(e.to_string()),
                Ok(Ok(found)) => match prefer_ipv4(&found) {
                    addresses if addresses.is_empty() => Err("no address".to_string()),
                    addresses => Ok(addresses),
                },
            };
            (
                name,
                Lookup {
                    result,
                    took: started.elapsed(),
                },
            )
        });
    }
    let mut found = BTreeMap::new();
    while let Some(joined) = tasks.join_next().await {
        if let Ok((name, lookup)) = joined {
            found.insert(name, lookup);
        }
    }
    found
}

/// The addresses a name last resolved to.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct CacheEntry {
    addresses: Vec<IpAddr>,
    resolved_at_unix_ms: u64,
}

/// `resolved-relays.json`: the last successful resolution of each name. An
/// entry stays until the name resolves again, however old it is.
#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Cache(BTreeMap<String, CacheEntry>);

impl Cache {
    async fn load(path: &Path) -> Cache {
        match tokio::fs::read_to_string(path).await {
            Ok(text) => serde_json::from_str(&text).unwrap_or_else(|e| {
                warn!("topology: ignoring unreadable {}: {e}", path.display());
                Cache::default()
            }),
            Err(e) if e.kind() == io::ErrorKind::NotFound => Cache::default(),
            Err(e) => {
                warn!("topology: cannot read {}: {e}", path.display());
                Cache::default()
            }
        }
    }
}

/// How one name ends up in the node's topology.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Source {
    Fresh(Vec<IpAddr>),
    Cached(Vec<IpAddr>),
    Name,
}

/// Replace each name that has addresses with one access point per address,
/// same port, in the same list; an address already in the list is not added
/// twice. Access points without a `port` and everything else are kept as they
/// are. A name absent from `addresses`
/// stays.
fn rewrite(topology: &mut Value, addresses: &BTreeMap<String, Vec<IpAddr>>) {
    fn rewrite_list(list: &mut Value, addresses: &BTreeMap<String, Vec<IpAddr>>) {
        let Some(points) = list.as_array_mut() else {
            return;
        };
        let mut out: Vec<Value> = Vec::with_capacity(points.len());
        let mut seen: BTreeSet<(String, String)> = BTreeSet::new();
        for point in points.drain(..) {
            let key = |p: &Value, address: &str| {
                (
                    address.to_string(),
                    p.get("port").map(Value::to_string).unwrap_or_default(),
                )
            };
            let name = point.get("address").and_then(Value::as_str);
            let has_port = point.get("port").is_some();
            match name.filter(|_| has_port).and_then(|n| addresses.get(n)) {
                Some(found) => {
                    for ip in found {
                        let text = ip.to_string();
                        if seen.insert(key(&point, &text)) {
                            let mut copy = point.clone();
                            copy["address"] = Value::String(text);
                            out.push(copy);
                        }
                    }
                }
                None => {
                    if let Some(a) = name {
                        seen.insert(key(&point, a));
                    }
                    out.push(point);
                }
            }
        }
        *points = out;
    }

    if let Some(list) = topology.get_mut("bootstrapPeers") {
        rewrite_list(list, addresses);
    }
    for group in ["localRoots", "publicRoots"] {
        if let Some(groups) = topology.get_mut(group).and_then(Value::as_array_mut) {
            for g in groups {
                if let Some(list) = g.get_mut("accessPoints") {
                    rewrite_list(list, addresses);
                }
            }
        }
    }
}

/// Every access point `address` in the topology that is not an IP literal.
/// An access point without a `port` is left to the node.
fn names_in(topology: &Value) -> BTreeSet<String> {
    fn collect(list: Option<&Value>, names: &mut BTreeSet<String>) {
        for point in list.and_then(Value::as_array).into_iter().flatten() {
            // An access point without a port is an SRV domain to the node.
            if point.get("port").is_none() {
                continue;
            }
            if let Some(address) = point.get("address").and_then(Value::as_str) {
                let literal = address.trim_start_matches('[').trim_end_matches(']');
                if literal.parse::<IpAddr>().is_err() {
                    names.insert(address.to_string());
                }
            }
        }
    }
    let mut names = BTreeSet::new();
    collect(topology.get("bootstrapPeers"), &mut names);
    for group in ["localRoots", "publicRoots"] {
        for g in topology
            .get(group)
            .and_then(Value::as_array)
            .into_iter()
            .flatten()
        {
            collect(g.get("accessPoints"), &mut names);
        }
    }
    names
}

/// Write `bytes` to `path` through a temporary file in the same directory, so
/// a reader sees the old file or the new one and never a partial one.
async fn write_atomic(path: &Path, bytes: &[u8]) -> io::Result<()> {
    let mut temporary = path.as_os_str().to_owned();
    temporary.push(".tmp");
    let temporary = PathBuf::from(temporary);
    let written = async {
        let mut file = tokio::fs::File::create(&temporary).await?;
        file.write_all(bytes).await?;
        file.sync_all().await?;
        drop(file);
        tokio::fs::rename(&temporary, path).await
    }
    .await;
    if written.is_err() {
        let _ = tokio::fs::remove_file(&temporary).await;
    }
    written
}

fn join(addresses: &[IpAddr]) -> String {
    addresses
        .iter()
        .map(IpAddr::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

/// Settle where each name's addresses come from: this start's lookup, else
/// the last successful one, else the name itself. Logs each name once.
fn choose_sources(
    names: &BTreeSet<String>,
    lookups: &BTreeMap<String, Lookup>,
    cache: &Cache,
) -> BTreeMap<String, Source> {
    names
        .iter()
        .map(|name| {
            let lookup = lookups.get(name);
            let took = lookup.map(|l| l.took.as_millis()).unwrap_or(0);
            let source = match lookup.map(|l| &l.result) {
                Some(Ok(addresses)) => {
                    info!(
                        "topology: {name} resolved to {} in {took} ms",
                        join(addresses)
                    );
                    Source::Fresh(addresses.clone())
                }
                failure => {
                    let why = match failure {
                        Some(Err(why)) => why.as_str(),
                        _ => "no result",
                    };
                    match cache.0.get(name) {
                        Some(entry) => {
                            warn!(
                                "topology: {name} did not resolve ({why}, {took} ms); \
                                 using cached addresses {} from {}",
                                join(&entry.addresses),
                                entry.resolved_at_unix_ms
                            );
                            Source::Cached(entry.addresses.clone())
                        }
                        None => {
                            warn!(
                                "topology: {name} did not resolve ({why}, {took} ms) \
                                 and has no cached addresses; leaving the name"
                            );
                            Source::Name
                        }
                    }
                }
            };
            (name.clone(), source)
        })
        .collect()
}

/// The node's arguments for this start. With a topology that holds names, the
/// names are resolved and `--topology` points at a copy of the topology with
/// addresses in their place; `args` itself is not changed. In every case the
/// result gives the node its roots: the arguments come back as they were when
/// there is nothing to resolve or the topology cannot be read or written.
async fn prepare_args<F, Fut>(
    args: &[String],
    state_dir: &str,
    limit: Duration,
    lookup: F,
) -> Vec<String>
where
    F: Fn(String) -> Fut,
    Fut: std::future::Future<Output = io::Result<Vec<IpAddr>>> + Send + 'static,
{
    let Some(topology_arg) = state::flag_value(args, "--topology") else {
        warn!("topology: the node arguments have no --topology; nothing to resolve");
        return args.to_vec();
    };
    // The node runs in its state directory, so a relative path is relative to it.
    let topology_path = Path::new(state_dir).join(topology_arg);
    let mut topology: Value = match tokio::fs::read_to_string(&topology_path).await {
        Ok(text) => match serde_json::from_str(&text) {
            Ok(v) => v,
            Err(e) => {
                warn!("topology: cannot parse {}: {e}", topology_path.display());
                return args.to_vec();
            }
        },
        Err(e) => {
            warn!("topology: cannot read {}: {e}", topology_path.display());
            return args.to_vec();
        }
    };
    let names = names_in(&topology);
    if names.is_empty() {
        info!("topology: {} holds no names", topology_path.display());
        return args.to_vec();
    }

    let cache_path = Path::new(state_dir).join(CACHE_FILE);
    let mut cache = Cache::load(&cache_path).await;
    let lookups = resolve_names(&names, limit, lookup).await;
    let sources = choose_sources(&names, &lookups, &cache);

    let now = unix_ms();
    let mut cache_changed = false;
    for (name, source) in &sources {
        if let Source::Fresh(addresses) = source {
            cache.0.insert(
                name.clone(),
                CacheEntry {
                    addresses: addresses.clone(),
                    resolved_at_unix_ms: now,
                },
            );
            cache_changed = true;
        }
    }
    if cache_changed {
        match serde_json::to_vec_pretty(&cache) {
            Ok(bytes) => {
                if let Err(e) = write_atomic(&cache_path, &bytes).await {
                    warn!("topology: cannot write {}: {e}", cache_path.display());
                }
            }
            Err(e) => warn!("topology: cannot serialize the address cache: {e}"),
        }
    }

    let addresses: BTreeMap<String, Vec<IpAddr>> = sources
        .into_iter()
        .filter_map(|(name, source)| match source {
            Source::Fresh(a) | Source::Cached(a) => Some((name, a)),
            Source::Name => None,
        })
        .collect();
    if addresses.is_empty() {
        return args.to_vec();
    }
    rewrite(&mut topology, &addresses);

    let output = Path::new(state_dir).join(RESOLVED_TOPOLOGY_FILE);
    let written = match serde_json::to_vec_pretty(&topology) {
        Ok(bytes) => write_atomic(&output, &bytes)
            .await
            .map_err(|e| e.to_string()),
        Err(e) => Err(e.to_string()),
    };
    if let Err(e) = written {
        warn!(
            "topology: cannot write {}: {e}; the node gets {}",
            output.display(),
            topology_path.display()
        );
        return args.to_vec();
    }
    info!("topology: the node gets {}", output.display());
    let mut patched = args.to_vec();
    state::replace_flag_value(&mut patched, "--topology", &output.to_string_lossy());
    patched
}

/// The arguments to start the node with: `node.args`, with `--topology`
/// pointing at the resolved copy when `node.resolve_topology` is set and the
/// topology holds names. `node.args` itself, which is persisted, is not
/// changed.
pub async fn node_args_for_start(node: &NodeConfig) -> Vec<String> {
    if !node.resolve_topology {
        return node.args.clone();
    }
    prepare_args(
        &node.args,
        &node.state_dir,
        Duration::from_millis(node.resolve_timeout_ms),
        os_lookup,
    )
    .await
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::sync::Arc;
    use std::time::{SystemTime, UNIX_EPOCH};

    struct TempDir(PathBuf);

    impl TempDir {
        fn new(label: &str) -> Self {
            let path = std::env::temp_dir().join(format!(
                "watchdog-topology-{}-{}-{}",
                label,
                std::process::id(),
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .subsec_nanos()
            ));
            std::fs::create_dir_all(&path).unwrap();
            TempDir(path)
        }

        fn path(&self) -> &Path {
            &self.0
        }

        fn str(&self) -> &str {
            self.0.to_str().unwrap()
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    fn ip(s: &str) -> IpAddr {
        s.parse().unwrap()
    }

    /// The shape of the Windows topology: ledger peers off, no peer
    /// snapshot, the bootstrap relays in both lists.
    fn windows_topology() -> Value {
        let relays = json!([
            {"address": "relay-a.example", "port": 3001},
            {"address": "relay-b.example", "port": 3002}
        ]);
        json!({
            "bootstrapPeers": relays,
            "localRoots": [{"accessPoints": [], "advertise": false, "valency": 1}],
            "publicRoots": [{"accessPoints": relays, "advertise": false}],
            "useLedgerAfterSlot": -1
        })
    }

    /// A lookup that answers from a table, and with an error for any name not
    /// in it.
    fn table(
        entries: &[(&str, &[&str])],
    ) -> impl Fn(String) -> std::future::Ready<io::Result<Vec<IpAddr>>> {
        let table: Arc<BTreeMap<String, Vec<IpAddr>>> = Arc::new(
            entries
                .iter()
                .map(|(n, a)| (n.to_string(), a.iter().map(|s| ip(s)).collect()))
                .collect(),
        );
        move |name| {
            std::future::ready(
                table
                    .get(&name)
                    .cloned()
                    .ok_or_else(|| io::Error::other("no such host")),
            )
        }
    }

    fn node_args(topology: &str) -> Vec<String> {
        ["run", "--topology", topology, "--config", "c.json"]
            .map(String::from)
            .to_vec()
    }

    fn write_topology(dir: &TempDir, topology: &Value) -> String {
        let path = dir.path().join("topology.yaml");
        std::fs::write(&path, serde_json::to_vec_pretty(topology).unwrap()).unwrap();
        path.to_string_lossy().into_owned()
    }

    fn read_json(path: &Path) -> Value {
        serde_json::from_slice(&std::fs::read(path).unwrap()).unwrap()
    }

    const LIMIT: Duration = Duration::from_millis(500);

    // ── prefer_ipv4 ─────────────────────────────────────────────────────────

    #[test]
    fn ipv4_is_preferred_when_a_name_has_any() {
        let found = [ip("2001:db8::1"), ip("192.0.2.1"), ip("192.0.2.2")];
        assert_eq!(prefer_ipv4(&found), [ip("192.0.2.1"), ip("192.0.2.2")]);
    }

    #[test]
    fn unspecified_addresses_are_dropped() {
        assert!(prefer_ipv4(&[ip("0.0.0.0"), ip("::")]).is_empty());
        assert_eq!(
            prefer_ipv4(&[ip("0.0.0.0"), ip("192.0.2.1")]),
            [ip("192.0.2.1")]
        );
        assert_eq!(
            prefer_ipv4(&[ip("::"), ip("2001:db8::1")]),
            [ip("2001:db8::1")]
        );
    }

    #[test]
    fn access_points_without_a_port_are_left_to_the_node() {
        let mut t = json!({"bootstrapPeers": [
            {"address": "srv.example"},
            {"address": "relay-a.example", "port": 1}
        ]});
        let names: Vec<String> = names_in(&t).into_iter().collect();
        assert_eq!(names, ["relay-a.example"]);
        rewrite(
            &mut t,
            &BTreeMap::from([
                ("srv.example".to_string(), vec![ip("192.0.2.9")]),
                ("relay-a.example".to_string(), vec![ip("192.0.2.1")]),
            ]),
        );
        assert_eq!(t["bootstrapPeers"][0], json!({"address": "srv.example"}));
        assert_eq!(t["bootstrapPeers"][1]["address"], "192.0.2.1");
    }

    #[tokio::test]
    async fn an_answer_of_only_unspecified_addresses_is_a_failure() {
        let names = BTreeSet::from(["blocked.example".to_string()]);
        let found = resolve_names(&names, LIMIT, |_| async {
            Ok(vec![ip("0.0.0.0"), ip("::")])
        })
        .await;
        assert!(found["blocked.example"].result.is_err());
    }

    #[test]
    fn ipv6_is_kept_when_a_name_has_no_ipv4() {
        let found = [ip("2001:db8::1"), ip("2001:db8::2"), ip("2001:db8::1")];
        assert_eq!(prefer_ipv4(&found), [ip("2001:db8::1"), ip("2001:db8::2")]);
    }

    // ── names_in and rewrite ────────────────────────────────────────────────

    #[test]
    fn names_are_the_addresses_that_are_not_ip_literals() {
        let t = json!({
            "bootstrapPeers": [
                {"address": "relay-a.example", "port": 1},
                {"address": "192.0.2.9", "port": 1},
                {"address": "2001:db8::9", "port": 1}
            ],
            "localRoots": [{"accessPoints": [{"address": "local.example", "port": 2}]}],
            "publicRoots": [{"accessPoints": [{"address": "[2001:db8::8]", "port": 3}]}]
        });
        let names: Vec<String> = names_in(&t).into_iter().collect();
        assert_eq!(names, ["local.example", "relay-a.example"]);
    }

    #[test]
    fn rewrite_replaces_names_in_every_list_and_keeps_the_rest() {
        let mut t = windows_topology();
        t["bootstrapPeers"][1]["extra"] = json!("kept");
        let addresses = BTreeMap::from([
            (
                "relay-a.example".to_string(),
                vec![ip("192.0.2.1"), ip("192.0.2.2")],
            ),
            ("relay-b.example".to_string(), vec![ip("2001:db8::b")]),
        ]);
        rewrite(&mut t, &addresses);
        let expected_relays = json!([
            {"address": "192.0.2.1", "port": 3001},
            {"address": "192.0.2.2", "port": 3001},
            {"address": "2001:db8::b", "port": 3002, "extra": "kept"}
        ]);
        assert_eq!(t["bootstrapPeers"], expected_relays);
        assert_eq!(
            t["publicRoots"][0]["accessPoints"]
                .as_array()
                .unwrap()
                .len(),
            3
        );
        assert_eq!(t["publicRoots"][0]["accessPoints"][0], expected_relays[0]);
        assert_eq!(t["publicRoots"][0]["advertise"], json!(false));
        assert_eq!(t["useLedgerAfterSlot"], json!(-1));
        assert!(t.get("peerSnapshotFile").is_none());
        assert_eq!(t["localRoots"], windows_topology()["localRoots"]);
    }

    #[test]
    fn rewrite_leaves_ip_literals_and_unresolved_names_alone() {
        let mut t = json!({"bootstrapPeers": [
            {"address": "192.0.2.9", "port": 1},
            {"address": "relay-a.example", "port": 1},
            {"address": "relay-b.example", "port": 1}
        ]});
        let before = t.clone();
        rewrite(&mut t, &BTreeMap::new());
        assert_eq!(t, before);
        rewrite(
            &mut t,
            &BTreeMap::from([("relay-a.example".to_string(), vec![ip("192.0.2.1")])]),
        );
        assert_eq!(t["bootstrapPeers"][0]["address"], "192.0.2.9");
        assert_eq!(t["bootstrapPeers"][1]["address"], "192.0.2.1");
        assert_eq!(t["bootstrapPeers"][2]["address"], "relay-b.example");
    }

    #[test]
    fn rewrite_does_not_repeat_an_address_in_one_list() {
        let mut t = json!({"bootstrapPeers": [
            {"address": "relay-a.example", "port": 1},
            {"address": "relay-b.example", "port": 1},
            {"address": "relay-c.example", "port": 2}
        ]});
        let shared = vec![ip("192.0.2.1")];
        rewrite(
            &mut t,
            &BTreeMap::from([
                ("relay-a.example".to_string(), shared.clone()),
                ("relay-b.example".to_string(), shared.clone()),
                ("relay-c.example".to_string(), shared),
            ]),
        );
        assert_eq!(
            t["bootstrapPeers"],
            json!([
                {"address": "192.0.2.1", "port": 1},
                {"address": "192.0.2.1", "port": 2}
            ])
        );
    }

    // ── resolve_names ───────────────────────────────────────────────────────

    #[tokio::test]
    async fn a_lookup_that_never_answers_fails_within_the_limit() {
        let names = BTreeSet::from(["silent.example".to_string(), "fast.example".to_string()]);
        let started = Instant::now();
        let found = resolve_names(&names, Duration::from_millis(100), |name| async move {
            if name == "silent.example" {
                std::future::pending::<io::Result<Vec<IpAddr>>>().await
            } else {
                Ok(vec![ip("192.0.2.1")])
            }
        })
        .await;
        assert!(started.elapsed() < Duration::from_secs(2));
        assert!(found["silent.example"].result.is_err());
        assert_eq!(found["fast.example"].result, Ok(vec![ip("192.0.2.1")]));
    }

    #[tokio::test]
    async fn an_empty_answer_is_a_failure() {
        let names = BTreeSet::from(["empty.example".to_string()]);
        let found = resolve_names(&names, LIMIT, |_| async { Ok(vec![]) }).await;
        assert!(found["empty.example"].result.is_err());
    }

    #[tokio::test]
    async fn the_system_resolver_resolves_localhost() {
        let found = os_lookup("localhost".to_string()).await.unwrap();
        assert!(found.iter().any(IpAddr::is_loopback));
    }

    // ── prepare_args ────────────────────────────────────────────────────────

    #[tokio::test]
    async fn names_become_addresses_in_a_copy_and_the_arguments_are_not_changed() {
        let dir = TempDir::new("copy");
        let original = write_topology(&dir, &windows_topology());
        let args = node_args(&original);
        let lookup = table(&[
            ("relay-a.example", &["2001:db8::a", "192.0.2.1"]),
            ("relay-b.example", &["192.0.2.2"]),
        ]);
        let patched = prepare_args(&args, dir.str(), LIMIT, lookup).await;

        let resolved = dir.path().join(RESOLVED_TOPOLOGY_FILE);
        assert_eq!(args, node_args(&original));
        assert_eq!(patched[1], "--topology");
        assert_eq!(patched[2], resolved.to_string_lossy());
        assert_eq!(patched[3..], args[3..]);
        let t = read_json(&resolved);
        assert_eq!(
            t["bootstrapPeers"],
            json!([
                {"address": "192.0.2.1", "port": 3001},
                {"address": "192.0.2.2", "port": 3002}
            ])
        );
        assert_eq!(t["publicRoots"][0]["accessPoints"], t["bootstrapPeers"]);
        assert_eq!(t["useLedgerAfterSlot"], json!(-1));
        assert_eq!(read_json(Path::new(&original)), windows_topology());
        assert!(!dir.path().join("topology-resolved.json.tmp").exists());
        assert!(!dir.path().join("resolved-relays.json.tmp").exists());
    }

    #[tokio::test]
    async fn a_relative_topology_path_is_relative_to_the_state_directory() {
        let dir = TempDir::new("relative");
        write_topology(&dir, &windows_topology());
        let args = node_args("topology.yaml");
        let lookup = table(&[
            ("relay-a.example", &["192.0.2.1"]),
            ("relay-b.example", &["192.0.2.2"]),
        ]);
        let patched = prepare_args(&args, dir.str(), LIMIT, lookup).await;
        assert_eq!(
            patched[2],
            dir.path().join(RESOLVED_TOPOLOGY_FILE).to_string_lossy()
        );
    }

    #[tokio::test]
    async fn a_name_that_does_not_resolve_uses_the_cached_addresses() {
        let dir = TempDir::new("cached");
        let original = write_topology(&dir, &windows_topology());
        let args = node_args(&original);
        let first = table(&[
            ("relay-a.example", &["192.0.2.1"]),
            ("relay-b.example", &["192.0.2.2"]),
        ]);
        prepare_args(&args, dir.str(), LIMIT, first).await;

        // relay-a no longer resolves; relay-b has new addresses.
        let second = table(&[("relay-b.example", &["192.0.2.22"])]);
        let patched = prepare_args(&args, dir.str(), LIMIT, second).await;
        let t = read_json(Path::new(&patched[2]));
        assert_eq!(
            t["bootstrapPeers"],
            json!([
                {"address": "192.0.2.1", "port": 3001},
                {"address": "192.0.2.22", "port": 3002}
            ])
        );
        let cache = read_json(&dir.path().join(CACHE_FILE));
        assert_eq!(cache["relay-a.example"]["addresses"], json!(["192.0.2.1"]));
        assert_eq!(cache["relay-b.example"]["addresses"], json!(["192.0.2.22"]));
    }

    #[tokio::test]
    async fn a_repeat_lookup_with_the_same_addresses_updates_the_time() {
        let dir = TempDir::new("retime");
        let original = write_topology(&dir, &windows_topology());
        let args = node_args(&original);
        let cache = json!({
            "relay-a.example": {"addresses": ["192.0.2.1"], "resolved_at_unix_ms": 1},
            "relay-b.example": {"addresses": ["192.0.2.2"], "resolved_at_unix_ms": 1}
        });
        std::fs::write(dir.path().join(CACHE_FILE), cache.to_string()).unwrap();
        let same = table(&[
            ("relay-a.example", &["192.0.2.1"]),
            ("relay-b.example", &["192.0.2.2"]),
        ]);
        prepare_args(&args, dir.str(), LIMIT, same).await;
        let cache = read_json(&dir.path().join(CACHE_FILE));
        for name in ["relay-a.example", "relay-b.example"] {
            assert!(cache[name]["resolved_at_unix_ms"].as_u64().unwrap() > 1);
        }
        assert_eq!(cache["relay-a.example"]["addresses"], json!(["192.0.2.1"]));
    }

    #[tokio::test]
    async fn a_name_with_no_cache_stays_a_name() {
        let dir = TempDir::new("name");
        let original = write_topology(&dir, &windows_topology());
        let args = node_args(&original);
        let only_b = table(&[("relay-b.example", &["192.0.2.2"])]);
        let patched = prepare_args(&args, dir.str(), LIMIT, only_b).await;
        let t = read_json(Path::new(&patched[2]));
        assert_eq!(
            t["bootstrapPeers"],
            json!([
                {"address": "relay-a.example", "port": 3001},
                {"address": "192.0.2.2", "port": 3002}
            ])
        );
        let cache = read_json(&dir.path().join(CACHE_FILE));
        assert!(cache.get("relay-a.example").is_none());
    }

    #[tokio::test]
    async fn when_no_name_resolves_the_node_gets_the_shipped_topology() {
        let dir = TempDir::new("none");
        let original = write_topology(&dir, &windows_topology());
        let args = node_args(&original);
        let patched = prepare_args(&args, dir.str(), LIMIT, table(&[])).await;
        assert_eq!(patched, args);
        assert!(!dir.path().join(RESOLVED_TOPOLOGY_FILE).exists());
    }

    #[tokio::test]
    async fn a_lookup_that_never_answers_falls_back_within_the_limit() {
        let dir = TempDir::new("silent");
        let original = write_topology(&dir, &windows_topology());
        let args = node_args(&original);
        let first = table(&[
            ("relay-a.example", &["192.0.2.1"]),
            ("relay-b.example", &["192.0.2.2"]),
        ]);
        prepare_args(&args, dir.str(), LIMIT, first).await;

        let started = Instant::now();
        let patched = prepare_args(&args, dir.str(), Duration::from_millis(100), |_| {
            std::future::pending::<io::Result<Vec<IpAddr>>>()
        })
        .await;
        assert!(started.elapsed() < Duration::from_secs(2));
        let t = read_json(Path::new(&patched[2]));
        assert_eq!(t["bootstrapPeers"][0]["address"], "192.0.2.1");
        assert_eq!(t["bootstrapPeers"][1]["address"], "192.0.2.2");
    }

    #[tokio::test]
    async fn cached_addresses_do_not_expire() {
        let dir = TempDir::new("old");
        let original = write_topology(&dir, &windows_topology());
        let cache = json!({
            "relay-a.example": {"addresses": ["192.0.2.1"], "resolved_at_unix_ms": 1},
            "relay-b.example": {"addresses": ["192.0.2.2"], "resolved_at_unix_ms": 1}
        });
        std::fs::write(dir.path().join(CACHE_FILE), cache.to_string()).unwrap();
        let patched = prepare_args(&node_args(&original), dir.str(), LIMIT, table(&[])).await;
        let t = read_json(Path::new(&patched[2]));
        assert_eq!(t["bootstrapPeers"][0]["address"], "192.0.2.1");
        assert_eq!(t["bootstrapPeers"][1]["address"], "192.0.2.2");
    }

    #[tokio::test]
    async fn an_unreadable_cache_is_ignored() {
        let dir = TempDir::new("badcache");
        let original = write_topology(&dir, &windows_topology());
        std::fs::write(dir.path().join(CACHE_FILE), "not json").unwrap();
        let lookup = table(&[
            ("relay-a.example", &["192.0.2.1"]),
            ("relay-b.example", &["192.0.2.2"]),
        ]);
        let patched = prepare_args(&node_args(&original), dir.str(), LIMIT, lookup).await;
        assert_eq!(
            read_json(Path::new(&patched[2]))["bootstrapPeers"][0]["address"],
            "192.0.2.1"
        );
        assert!(
            read_json(&dir.path().join(CACHE_FILE))
                .get("relay-a.example")
                .is_some()
        );
    }

    #[tokio::test]
    async fn a_topology_without_names_is_left_alone() {
        let dir = TempDir::new("literals");
        let t = json!({"bootstrapPeers": [{"address": "192.0.2.1", "port": 1}]});
        let original = write_topology(&dir, &t);
        let args = node_args(&original);
        assert_eq!(
            prepare_args(&args, dir.str(), LIMIT, table(&[])).await,
            args
        );
        assert!(!dir.path().join(RESOLVED_TOPOLOGY_FILE).exists());
        assert!(!dir.path().join(CACHE_FILE).exists());
    }

    #[tokio::test]
    async fn arguments_without_a_readable_topology_are_left_alone() {
        let dir = TempDir::new("unreadable");
        let none: Vec<String> = vec!["run".to_string()];
        assert_eq!(
            prepare_args(&none, dir.str(), LIMIT, table(&[])).await,
            none
        );
        let missing = node_args(&dir.path().join("absent.json").to_string_lossy());
        assert_eq!(
            prepare_args(&missing, dir.str(), LIMIT, table(&[])).await,
            missing
        );
        std::fs::write(dir.path().join("bad.json"), "{").unwrap();
        let bad = node_args(&dir.path().join("bad.json").to_string_lossy());
        assert_eq!(prepare_args(&bad, dir.str(), LIMIT, table(&[])).await, bad);
    }

    #[tokio::test]
    async fn a_second_start_replaces_the_resolved_file() {
        let dir = TempDir::new("replace");
        let original = write_topology(&dir, &windows_topology());
        let args = node_args(&original);
        let first = table(&[
            ("relay-a.example", &["192.0.2.1"]),
            ("relay-b.example", &["192.0.2.2"]),
        ]);
        prepare_args(&args, dir.str(), LIMIT, first).await;
        let second = table(&[
            ("relay-a.example", &["192.0.2.101"]),
            ("relay-b.example", &["192.0.2.102"]),
        ]);
        let patched = prepare_args(&args, dir.str(), LIMIT, second).await;
        assert_eq!(
            read_json(Path::new(&patched[2]))["bootstrapPeers"][0]["address"],
            "192.0.2.101"
        );
    }

    // ── node_args_for_start ─────────────────────────────────────────────────

    fn node_config(args: Vec<String>, state_dir: &str, resolve: bool) -> NodeConfig {
        serde_json::from_value(json!({
            "exe": "node", "args": args, "state_dir": state_dir,
            "socket_path": "s", "resolve_topology": resolve
        }))
        .unwrap()
    }

    #[tokio::test]
    async fn without_the_flag_the_arguments_are_the_configured_ones() {
        let dir = TempDir::new("flagless");
        let original = write_topology(&dir, &windows_topology());
        let node = node_config(node_args(&original), dir.str(), false);
        assert_eq!(node_args_for_start(&node).await, node.args);
        assert!(!dir.path().join(CACHE_FILE).exists());
    }

    #[tokio::test]
    async fn with_the_flag_a_topology_of_localhost_becomes_loopback_addresses() {
        let dir = TempDir::new("localhost");
        let t = json!({"bootstrapPeers": [{"address": "localhost", "port": 3001}]});
        let original = write_topology(&dir, &t);
        let node = node_config(node_args(&original), dir.str(), true);
        let patched = node_args_for_start(&node).await;
        let resolved = read_json(Path::new(&patched[2]));
        let address: IpAddr = resolved["bootstrapPeers"][0]["address"]
            .as_str()
            .unwrap()
            .parse()
            .unwrap();
        assert!(address.is_loopback());
        assert_eq!(resolved["bootstrapPeers"][0]["port"], 3001);
    }
}
