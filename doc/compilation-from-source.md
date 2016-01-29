Use the following instructions to compile Fancydiff from this source code repository.

### On Linux

#### Dependencies

For Fedora/CentOS, do
```
sudo yum install libicu-devel zlib-devel openssl-devel gmp-devel pcre-devel
```

For Debian/Ubuntu, do
```
sudo apt-get install libicu-dev libz-dev libssl-dev libgmp-dev
```

Download and install [haskell-stack](http://docs.haskellstack.org/en/stable/install_and_upgrade.html) for Linux,


#### Build

In the Git clone of Fancydiff, do the following:

```
stack setup
stack install
```


### On Mac OS X

#### Dependencies

Download [haskell-stack](http://docs.haskellstack.org/en/stable/install_and_upgrade.html#mac-os-x) for MacOS X,
and do the following:

```
brew install icu4c
brew install openssl
```

#### Build and install

Change to the cloned repo of Fancydiff and do the following:

```
stack setup
stack install \
    --extra-include-dirs=/usr/local/opt/icu4c/include   \
    --extra-lib-dirs=/usr/local/opt/icu4c/lib           \
    --extra-include-dirs=/usr/local/opt/openssl/include \
    --extra-lib-dirs=/usr/local/opt/openssl/lib
```
