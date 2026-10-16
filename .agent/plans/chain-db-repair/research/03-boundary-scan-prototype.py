"""Prototype: check hash linkage across ImmutableDB chunk boundaries.

Usage: python3 03-boundary-scan-prototype.py <chain>/immutable <first chunk> <last chunk>

For every non-empty chunk, compare the previous-hash field in the header of its
first block with the header hash stored in the last secondary-index entry of
the most recent non-empty chunk before it. A mismatch means blocks are missing
between the two chunks. Empty chunks are skipped and the comparison carries
across them. Byron headers have a different layout and are counted as skipped.

Secondary index entry layout (56 bytes, big-endian):
  blockOffset u64 | headerOffset u16 | headerSize u16 | checksum u32 |
  headerHash 32 bytes | blockOrEBB u64

Read-only. Reads 56 bytes at each end of every .secondary file (the prototype
reads the whole file for simplicity) and one header from each .chunk file.
"""
import hashlib
import struct
import sys
import time

ENTRY = 56


def entry(raw):
    boff, hoff, hsz, crc = struct.unpack(">QHHI", raw[:16])
    return boff, hoff, hsz, crc, raw[16:48], struct.unpack(">Q", raw[48:56])[0]


def read_uint(b, i):
    ai = b[i] & 0x1F
    if ai < 24:
        return ai, i + 1
    n = {24: 1, 25: 2, 26: 4, 27: 8}[ai]
    return int.from_bytes(b[i + 1 : i + 1 + n], "big"), i + 1 + n


def prev_hash(header):
    # header = [header_body, kes_sig]; header_body = [block_no, slot, prev_hash, ...]
    if header[0] != 0x82:
        return None, None
    i = 1
    if header[i] >> 5 != 4:  # header_body must be an array
        return None, None
    i += 1
    _block_no, i = read_uint(header, i)
    slot, i = read_uint(header, i)
    if header[i] == 0x58 and header[i + 1] == 0x20:
        return header[i + 2 : i + 34], slot
    return None, slot


def main():
    d, start, end = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
    t0 = time.time()
    ok = bad = skipped = empty = selfhash_ok = selfhash_bad = 0
    prev_last = None  # header hash of the last block in the most recent non-empty chunk
    for n in range(start, end + 1):
        with open(f"{d}/{n:05d}.secondary", "rb") as f:
            raw = f.read()
        if not raw:
            empty += 1
            continue
        first, last = entry(raw[:ENTRY]), entry(raw[-ENTRY:])
        if prev_last is not None:
            with open(f"{d}/{n:05d}.chunk", "rb") as f:
                f.seek(first[0] + first[1])
                header = f.read(first[2])
            ph, slot = prev_hash(header)
            if ph is None:
                skipped += 1
            else:
                if hashlib.blake2b(header, digest_size=32).digest() == first[4]:
                    selfhash_ok += 1
                else:
                    selfhash_bad += 1
                if ph == prev_last:
                    ok += 1
                else:
                    bad += 1
                    print(f"BREAK before chunk {n} (first slot of {n}: {slot})")
        prev_last = last[4]
    dt = time.time() - t0
    print(f"chunks {start}..{end}: linked={ok} broken={bad} skipped(non-post-Byron)={skipped} empty={empty} "
          f"selfhash_ok={selfhash_ok} selfhash_bad={selfhash_bad} seconds={dt:.1f}")


if __name__ == "__main__":
    main()
