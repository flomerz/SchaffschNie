# Requirements
## 1. Ubuntu prerequisites
```bash
# Multiprecision arithmetic library developers tools, zlib  
sudo apt-get install libgmp-dev zlib1g-dev -y
```

## 2. clean-up
```bash
# remove old  
rm -rf ~/.cabal ~/.ghc
```

## 3. set path
Exports in ~/.profile definieren:
```bash
# add this lines to ~/.profile  
export GHC_HOME=/opt/haskell/ghc
export CABAL_HOME=~/.cabal
export STACK_HOME=/opt/haskell/stack
export PATH=$GHC_HOME/bin:$CABAL_HOME/bin:$STACK_HOME:$PATH
```
reboot or do:
```bash
source ~/.profile
```

## 4. GHC
Homepage: https://www.haskell.org/ghc  
Source download: http://downloads.haskell.org/%7Eghc/7.10.2/ghc-7.10.2-x86_64-unknown-linux-deb7.tar.bz2  
```bash
tar xfj ghc-7.10.2-x86_64-unknown-linux-deb7.tar.bz2
cd ghc-7.10.2

# install to
sudo mkdir -p /opt/haskell/ghc

# parallel 4 jobs
sudo ./configure --prefix=/opt/haskell/ghc && sudo make -j 4 install
```

## 5. Cabal (Package Manager für Haskell)
Homepage: https://www.haskell.org/cabal  
### 1. cabal
Source download: https://www.haskell.org/cabal/release/cabal-1.22.4.0/Cabal-1.22.4.0.tar.gz  
```bash
tar xzf Cabal-1.22.4.0.tar.gz
cd Cabal-1.22.4.0

# build
ghc --make Setup.hs && ./Setup configure --user && ./Setup build && ./Setup install
cabal-install
Source download: https://www.haskell.org/cabal/release/cabal-install-1.22.6.0/cabal-install-1.22.6.0.tar.gz

tar xzf cabal-install-1.22.6.0.tar.gz
cd cabal-install-1.22.6.0

# install  
./bootstrap.sh
```

### 2. cabal update
Stack (neuer Package Manager und Build Tool)  
Homepage: https://www.stackage.org/  
Bin download: https://github.com/commercialhaskell/stack/releases/download/v0.1.5.0/stack-0.1.5.0-x86_64-linux.tar.gz  
```bash
tar xfz stack-0.1.5.0-x86_64-linux.tar.gz
sudo mv stack-0.1.5.0-x86_64-linux /opt/haskell/stack

sudo apt-get install libtinfo-dev
```

## 6. SDL (OpenGL library)
### 1. SDL2
Homepage: https://www.libsdl.org  
Source download: https://www.libsdl.org/release/SDL2-2.0.3.tar.gz  
```bash
tar xfz SDL2-2.0.3.tar.gz
cd SDL2-2.0.3

# install
./configure && make -j 4 && sudo make install
```

### 2. SDL2-TTF (Font)
Homepage: https://www.libsdl.org/projects/SDL_ttf/  
Source download: https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-2.0.12.tar.gz  
```bash
sudo apt-get installl libsdl2-ttf-dev
```

## 7. SublimeText Haskell
Get Sublime Text 2: http://www.sublimetext.com/  
Install the Sublime Package Control package: http://wbond.net/sublime_packages/package_control/installation  
Use Package Control to install this package (SublimeHaskell)  
```bash
cabal install aeson happy haskell-src-exts haddock
cabal install ghc-mod stylish-haskell hdevtools
```
