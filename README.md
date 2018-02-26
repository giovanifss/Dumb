# Dumb - Dumain Bruteforcer

A tool to bruteforce dumains!

![Dumb](http://8449-presscdn-0-66.pagely.netdna-cdn.com/wp-content/uploads/2013/07/dumb.jpg)

## How DUMB works:
Dumb works with a masked dumain for substitution. The dumain can have as many masks as you want as long as you pass the according wordlists. For example:  

#### Bruteforcing subdumains:
Using the mask `DUMB.dumain.com` and the following wordlists:  
```
www
ftp
backoffice
```
Dumb will generate the following dumains for bruteforce:
```
www.dumain.com
ftp.dumain.com
backoffice.dumain.com
```
For subdumains, you can only pass `dumain.com` and dumb will understand as `DUMB.dumain.com`.

#### Bruteforcing domain endings:
Using the same principle, you can pass as mask `dumain.DUMB` with the following wordlist:
```
com
net
org
```
Dumb will generate the following dumains for bruteforce:
```
dumain.com
dumain.net
dumain.org
```

#### Bruteforcing everything:
To bruteforce **everything** you can pass the mask as "DUMB.DUMB.DUMB" passing three wordlists:
```
wordlist1   wordlist2   wordlist3
www         foo         com
ftp         bar         net
```
Dumb will generate:
```
www.foo.com
ftp.foo.com
www.bar.com
ftp.bar.com
www.foo.net
ftp.foo.net
www.bar.net
ftp.bar.net
```

## Usage:
Dumb receives the dumain mask as first parameter and the wordlists following. The number of wordlists must match the number of masks in the dumain. For example:
- One mask:  
`$ dumb "DUMB.dumain.com" wordlists/foo.txt`
- Two masks:  
`$ dumb "DUMB.dumain.DUMB" wordlists/foo.txt wordlists/bar.txt`
- Several masks:  
`$ dumb "DUMB-DUMB-DUMB_DUMB.DUMB.DUMB" wordlists/foo_1.txt ... wordlists/foo_6.txt`

## Docker:
If you don't want to build from source, you can use the docker version: `docker run -it giovanifss/dumb "DUMB.dumain.com" subdomains.txt`  
**All the wordlists in wordlists/ are inside the docker container in filesystem root (/)**, this means that you can call dumb passing the wordlists name: `docker run -it giovanifss/dumb "DUMB.dumain.com" (subdomains.txt|subdominios.txt|domain-endings.txt)`

To work with local wordlists that aren't present inside the container, you can use docker volumes:  
`docker run -v local/wordlist.txt:/opt/wordlist.txt -it giovanifss/dumb "DUMB.dumain.com" /opt/wordlist.txt`  

## Building from source:
If you want to build from source you will need [stack](https://docs.haskellstack.org/en/stable/README/):  
- Enter in the project directory and run `$ stack build`.  
- To execute: `$ stack exec dumb "DUMB.dumain.com" wordlists/subdomains.txt`  
Note that some older versions of stack have some problems to build the project (Debian stack package, for example). Make sure you get the latest stack version.

## Future features:
Future planned features are:  
- Argument parser support, for better configuration of the tool execution;
- Post analysis of found dumains, generating statistics and metrics;

## Performance:
The tool performance will highly depend on your network connection. Usually, it should take less then 10 seconds to finish a subdumain burteforce with the `wordlists/subdomains.txt` wordlist.  

If you have a good connection and think that the tool is slow, try changing the `1000` in the `splitDomains` function call, e.g. `mapM_ (MP.mapM_ (resolve rs)) (splitDomains 1000 allDomains)`, to a higher value.  

Alternatively, you can change `mapM_ (MP.mapM_ (resolve rs)) (splitDomains 1000 allDomains)` to `MP.mapM_ (resolve rs) allDomains` to execute all the requests in parallel.
