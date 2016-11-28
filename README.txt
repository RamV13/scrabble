
How to Run (publicly):
- navigate to the address `http://vellanki-web.coecis.cornell.edu/` in Chrome

How to Run (locally):

  Install Dependencies:
    - `opam install ounit yojson cohttp js_of_ocaml lwt`
    - `brew install node` 
      (dependent 'node_modules' are packaged into the submission for simplicity)
    * use `opam update` and `opam upgrade` to fix unbound value compile errors

  Obtain Host IP Address:
    - `ifconfig`
    - record the `inet` address that is not '127.0.0.1' (localhost)
      (should be of the form 10.X.X.X or 192.168.X.X)

  Compile (all `cd` commands are with respect to the main project directory):
    - `cd public/js`
    - Modify the 'baseURL' variable on line 23 of 'scrabbleClient.ml' to the 
      value: `http://<HOST_IP_ADDRESS>` with the IP address obtained above
      (this is necessary because the default value directs HTTP client requests
       to the public server which is used in the instructions above on 'How to
       Run (publicly)' and also because more time would be needed to convert
       this variable to a runtime argument/parameter)
    - `make` (use `make clean` then `make` to rebuild)
    - `cd server`
    - `make` (use `make clean` then `make` to rebuild)

  Run (all `cd` commands are with respect to the main project directory):
    - `cd server`
    - `./server.byte`
    - open a new Terminal tab
    - `sudo node server.js`
    - open a new Terminal tab
    - use the `inet` address as the URL to access the web application from other 
      machines on the same WiFi network
    - navigate to the `inet` address in Chrome
