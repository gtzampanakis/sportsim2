#!/bin/bash

rm db.db
guile \
  -L . \
  -L ~/dev/guile-sqlite3/modules/ \
  main4.scm
