# Crow
Create unique wordlists from list of urls.

## Usage
Fetch a couple of millions urls from wayback machine, then feed them to 'Crow'. It will return you 3 wordlist files with previously used directories, arguments and it's values.

### Roswell
If installed with roswell, simply call
```
$ crow urls.list dirs.list args.list args-values.list
```

### From lisp
```
(crow:collect-from-file "urls.list" "dirs.list" "args.list" "args-values.list")
```

## Installation
The easiest way is install with roswell.

### Roswell
```
ros install walpurgisnatch/crow
```

## License

Licensed under the MIT License.

## Copyright

Copyright (c) 2021 Walpurgisnatch
