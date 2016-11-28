
TODO differentiate between public and local server running

How to Run (publicly):
- navigate to the address `http://vellanki-web.coecis.cornell.edu/` in Chrome

How to Run (locally):
- `opam install ounit yojson cohttp js_of_ocaml lwt`
- `cd public/js`
- `make` (use `make clean` then `make` to rebuild)
- `cd ../../server`
- `make` (use `make clean` then `make` to rebuild)
- `./server.byte`
- open a new Terminal tab
- `cd ../`
- `sudo node server.js`
- open a new Terminal tab
- `ifconfig`
- use the `inet` address as the URL to access the web application from other 
  machines on the same WiFi network
- navigate to the `inet` address in Chrome

use `opam update` and `opam upgrade` to fix unbound value compile errors
