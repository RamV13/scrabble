
How to Access (publicly):
- connect to Cornell's WiFi Network (i.e. eduroam)
- navigate to the address `http://vellanki-web.coecis.cornell.edu/` in Chrome

How to Run (locally):

  Install Dependencies:
    - `opam install ounit yojson cohttp js_of_ocaml lwt`
    - `brew install node` 
      (dependent 'node_modules' are packaged into the submission for simplicity
       so there is no need for an `npm install` command)
    * use `opam update` and `opam upgrade` to fix unbound value compile errors

  Obtain Host IP Address:
    - `ifconfig`
    - record the 'inet' address that is not '127.0.0.1' (localhost)
      (should be of the form 10.X.X.X or 192.168.X.X)

  Compile (all `cd` commands are with respect to the main project directory):
    - `cd public/js`
    - `make` (use `make clean` then `make` to rebuild if necessary)
    - `cd server`
    - `make` (use `make clean` then `make` to rebuild if necessary)

  Run (all `cd` commands are with respect to the main project directory):
    - `cd server`
    - `./server.byte`
    - open a new Terminal tab
    - `npm start` (from the main project directory)
    - enter your password
    - navigate to the 'inet' address in Chrome (this is the URL to access the 
      web application from other machines on the same WiFi network)
