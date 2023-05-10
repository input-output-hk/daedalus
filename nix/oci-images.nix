{ inputs, targetSystem }:

let pkgs = inputs.nixpkgs.legacyPackages.${targetSystem}; in

if targetSystem == "x86_64-linux" then {

  coinGeckoProxy = let
    nginx = pkgs.nginxMainline;
    nginxCmd = { listenPort }: [ "${nginx}/bin/nginx" "-c" (nginxConf { inherit listenPort; }) ];
    nginxConf = { listenPort }: let
      cacheZone = "one";
      nginxConf = ''
        daemon off;
        user root root;
        error_log stderr notice;
        events {
          worker_connections 1024;
        }
        http {
          access_log /dev/stdout;
          include ${nginx}/conf/mime.types;
          default_type  application/octet-stream;
          sendfile on;
          keepalive_timeout 65;
          proxy_cache_path /var/cache/nginx/${cacheZone} keys_zone=${cacheZone}:16m inactive=10m max_size=256m;
          resolver 8.8.8.8 8.8.4.4;
          server {
            listen ${toString listenPort};
            server_name localhost;
            location / {
              return 404;
            }
            error_page 500 502 503 504  /50x.html;
            location = /50x.html {
              root ${nginx}/html;
            }
            location = /api/v3/coins/list {
              ${proxyConf}
              proxy_cache_valid 200 10m;
              proxy_cache_valid any 1m;
              set $coingecko https://api.coingecko.com;
              proxy_pass $coingecko;
            }
            location = /api/v3/simple/supported_vs_currencies {
              ${proxyConf}
              proxy_cache_valid 200 10m;
              proxy_cache_valid any 1m;
              set $coingecko https://api.coingecko.com;
              proxy_pass $coingecko;
            }
            location = /api/v3/coins/markets {
              if ( $args !~ ^ids=cardano&vs_currency=[a-z0-9]+$ ) { return 400; }
              ${proxyConf}
              proxy_cache_valid 200 400 10m;
              proxy_cache_valid any 1m;
              set $coingecko https://api.coingecko.com;
              proxy_pass $coingecko;
            }
            location = /api/v3/simple/price {
              if ( $args !~ ^ids=(cardano|bitcoin)&vs_currencies=[a-z0-9,]+(&include_last_updated_at=(true|false))?(&include_24hr_change=(true|false))?$ ) { return 400; }
              ${proxyConf}
              proxy_cache_valid 200 400 10m;
              proxy_cache_valid any 1m;
              set $coingecko https://api.coingecko.com;
              proxy_pass $coingecko;
            }
          }
        }
      '';
      proxyConf = ''
        proxy_cache ${cacheZone};
        proxy_cache_lock on;
        proxy_cache_lock_age 10s;
        proxy_cache_lock_timeout 10s;
        proxy_connect_timeout 60s;
        # Don’t let our clients control caching behavior:
        proxy_cache_revalidate off;
        proxy_cache_use_stale error timeout invalid_header updating
                              http_500 http_502 http_503 http_504
                              http_403 http_404 http_429;
        proxy_cache_background_update on;
        # Hide some original headers:
        proxy_hide_header x-request-id;
        proxy_hide_header x-runtime;
        proxy_hide_header cf-ray;
        proxy_hide_header set-cookie;
        proxy_hide_header age;
        proxy_hide_header expires;
        proxy_hide_header etag;
        proxy_hide_header alternate-protocol;
        proxy_hide_header cf-cache-status;
        # Don’t close upstream connections if our client closes before getting a response:
        proxy_ignore_client_abort on;
        # Ignore all upstream fields that could interfere with our caching policies:
        proxy_ignore_headers X-Accel-Redirect X-Accel-Expires X-Accel-Limit-Rate
                             X-Accel-Buffering X-Accel-Charset
                             Expires Cache-Control Set-Cookie Vary;
        # Hide errors ≥300:
        proxy_intercept_errors on;
        proxy_pass_request_body off;
        proxy_set_header Content-Length "";
        proxy_pass_request_headers off;
        proxy_ssl_verify on;
        proxy_ssl_trusted_certificate /etc/ssl/certs/ca-bundle.crt;
        proxy_ssl_server_name on;
        add_header X-Cache-Status $upstream_cache_status;
        proxy_set_header User-Agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Safari/537.36";
      '';
    in pkgs.writeText "nginx-${toString listenPort}.conf" nginxConf;
    entrypoint = pkgs.writeShellScript "entrypoint" ''
      set -euo pipefail
      export PATH="${with pkgs; lib.makeBinPath [ coreutils ]}:$PATH"

      # Let’s pre-populate the cache with the most popular requests:

      ${pkgs.lib.escapeShellArgs (nginxCmd { listenPort = 81; })} &
      initial_pid=$!

      for endpoint in \
        '/api/v3/coins/list' \
        '/api/v3/simple/supported_vs_currencies' \
        '/api/v3/coins/markets?ids=cardano&vs_currency=usd' \
        '/api/v3/coins/markets?ids=cardano&vs_currency=jpy' \
        '/api/v3/simple/price?ids=cardano&vs_currencies=usd&include_last_updated_at=true' \
        '/api/v3/simple/price?ids=cardano&vs_currencies=usd&include_24hr_change=true' \
        ; do
        sleep 2
        echo "Will pre-populate cache with: ‘$endpoint’…"
        sleep 3
        ${pkgs.curl}/bin/curl >/dev/null -sS "http://127.0.0.1:81$endpoint" && echo '  ok' || echo '  failed'
      done

      kill -s TERM $initial_pid
      exec ${pkgs.lib.escapeShellArgs (nginxCmd { listenPort = 80; })}
    '';
  in pkgs.dockerTools.buildImage rec {
    name = "coingecko-proxy";
    tag = inputs.self.rev or "dirty";
    copyToRoot = with pkgs.dockerTools; [ caCertificates fakeNss ];   # XXX: needed for nginx
    runAsRoot = ''
      #!/bin/sh
      set -euo pipefail
      mkdir -p /var/log/nginx /var/cache/nginx
      exec ${pkgs.lib.escapeShellArgs (nginxCmd { listenPort = 80; })} -t
    '';
    config.Expose = "80/tcp";
    config.Entrypoint = [entrypoint];
  };

} else {}
