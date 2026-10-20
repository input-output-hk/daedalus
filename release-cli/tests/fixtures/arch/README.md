Public artifacts fetched from https://daedalus.se7enlabs.org/arch/ on 2026-10-20:

- `daedalus.db`: exact published gzip archive (608 bytes).
- `daedalus.db.asc`: published `daedalus.db.tar.gz.sig`, an armored signature.
- `daedalus-release.gpg`: published armored public key.

These contain no private key material. The signature covers `daedalus.db` and
identifies primary key `31B3F22FA0CCE2E8382C04A2807F2ACD0171BC90` and signing
subkey `CD1F21F62477EB42D7C4F3DEB3AD9541F4DBD0FE`. The regression test checks
the production identity policy against this real signature without a secret key.
