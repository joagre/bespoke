# Vendor dependencies

## libsodium

Download libsodium and build it for ARM:

```
tar zxvf libsodium-1.0.16.tar.gz
cd libsodium-1.0.16
sudo apt install gcc-aarch64-linux-gnu g++-aarch64-linux-gnu
mkdir libsodium-arm
./configure --host=aarch64-linux-gnu --prefix=$(cd ../libsodium-arm; pwd)
make
make install
```
