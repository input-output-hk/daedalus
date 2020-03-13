:warning: You will need `jcli` and `jormungandr` from https://github.com/input-output-hk/jormungandr

## How to generate a new genesis block 

```bash
$ jcli genesis encode --input genesis.yaml --output block0.bin
```

## How to launch jormungandr


```bash
$ jormungandr --genesis-block block0.bin --config config.yaml --secret secret.yaml
```

## How to generate keys (to put in genesis.yaml and secret.yaml)

```bash
$ jcli key generate --type=ed25519extended > key.prv
$ cat key.prv | jcli key to-public > key.pub
```
