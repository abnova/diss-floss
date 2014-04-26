#!/usr/bin/awk -f

BEGIN {
  first_pass = true;
  param = "\"\*[a-zA-Z^0-9]+?\"";
  regex = "\$\{[a-zA-Z^0-9]+?\}";
  params[""] = 0;
}

{
  if (first_pass)
    if (match($0, param)) {
      print(substr($0, RSTART, RLENGTH));
      params[param] = substr($0, RSTART, RLENGTH);
    }
  else
      gsub(regex, params[regex], $0);
}

END {
  if (first_pass) {
    ARGC++;
    ARGV[ARGIND++] = FILENAME;
    first_pass = false;
    nextfile;
  }
}
