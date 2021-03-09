This directory contains `.envrc` file that sets up variables for access to frag
and its database using [direnv][direnv]. To make it work, you will need direnv
and allow it for the current directory (`direnv allow`). Direnv will
automatically set-up the variable one you change to this directory.
Alternatively, you can source `.envrc` manually with `source .envrc`.

## Authentication

If you FI username does not match your local username, create a file `local.rc`
with contents `FRAG_USER=FI_XLOGIN` (e.g., `FRAG_USER=xstill`). This file will
be automatically loaded by `.envrc`.

You will also need to initialize Kerberos (unless you are logged in with an
active ticket, see `klist`). You can do this by running `kinit` (or `kinit
XLOGIN` if you FI login does not match you local login).

## Access Outside FI

If you want to access the database outside FI, you will need to connect to
[faculty VPN][vpn], we recommend using the split tunelling option (for FI) to
prevent routing all internet access through FI. On top of that, you will need
to install kerberos client (`krb5-user` for Debian/Ubuntu, `krb5` on Archlinux)
and set the config (`/etc/krb5.conf`) to at least the following settings (if
you already use Kerberos, you will probably not want to change `default_realm`
and you would like to keep your current entries):

```
[libdefaults]
	default_realm = "FI.MUNI.CZ"
	encrypt = true

[realms]
	FI.MUNI.CZ = {
		kdc = krb.fi.muni.cz
		kdc = krb1.fi.muni.cz
		admin_server = krb.fi.muni.cz
	}

[domain_realm]
	.fi.muni.cz = FI.MUNI.CZ
	fi.muni.cz = FI.MUNI.CZ
```

[direnv]: https://direnv.net/#basic-installation
[vpn]: https://www.fi.muni.cz/tech/unix/vpn.html
