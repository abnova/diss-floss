#!/usr/bin/gawk -f

BEGIN {
  param = "\"\\*([a-zA-Z]+?)\":\"([^\"]*)\"";
  regex = "\\${([a-zA-Z]+?)}";
}

NR == FNR {
    if (match($0, param, a)) {
      params[a[1]] = a[2];
    }
    next;
}

match($0, regex, a) {
  gsub(regex, params[a[1]], $0);
}
1
