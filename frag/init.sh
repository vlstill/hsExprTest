#!/usr/bin/env bash
frag-teacher init --schema --ldap
KEY=$(cat isnotebooks.key)
frag-teacher init --roles --is-sync --is-key $KEY
