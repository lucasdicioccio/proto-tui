# proto-tui

A terminal-UI for exploring Protobuf.

This tool is an early-stage demonstration spawning from an excuse to learn
`brick`, a Terminal-UI framework written in Haskell.

## Installation

Currently, only source-based installation is supported.

### Prerequisites

A prerequisite is to have `protoc`, the official Protobuf compiler somewhere in
your PATH.

### Compiling from sources

Currently, the only way to get a working `proto-tui` is building from source.
After installing Haskell `stack` tool and cloning the source locally, run
`stack install` in the directory of this README.

## How does it work?

This tool basically invokes `protoc` using the `-o` argument to generate a
binary-encoded description of the `.proto` compiled files. Then `proto-tui`
parses the binary-encoded description (which happens to itself use Protobuf
serialization) and provides a simple UI over it.

## Why would I use this tool?

This tool can help you explore and get familiar with large APIs (see examples).

This tool can also give you a better understanding of how `protoc` works under
the hood. For instance, to help you write your own `protoc` compiler plugins.

## Examples

### Explore GoogleApis the geeky way

Google exposes its public services via gRPC.

```shell
git clone https://github.com/googleapis/googleapis
cd googleapis
find . -name "*.proto" > all-protos
proto-tui-exe @all-protos
```

### Explore TensorFlow internal models

Tensorflow internally uses a lot of Protobuf structures.

```shell
git clone https://github.com/tensorflow/tensorflow.git
cd tensorflow
find . -name "*.proto" > all-protos
proto-tui-exe @all-protos
```
