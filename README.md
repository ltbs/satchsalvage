# satchsalvage

Provides a simple CLI driven interface to retrieve data from an old Satchmo Ecommerce system and place it in a hierarchical directory structure.
Product descrptions, titles, slugs, skus, attributes and images can all be retrieved. 
Designed to work on Linux and not tested on any other OS.  

Product variations are created as subdirectories of the parent product, and category directories contain symlinks to each of the products they contain.

```
Usage: satchsalvage DATABASE [-d|--dir DIRECTORY]
  Rescues data from a satchmo db into the filesystem. Optionally also fetches
  images from the static directory.

Available options:
  -h,--help                Show this help text
  DATABASE                 MySQL Database to extract from
  -d,--dir DIRECTORY       Path to static directory in satchmo to extract images
                           from
```
