This repo contains my personal Emacs and Bash configurations files.

# Layout

- `init.el` — Emacs entry point template. **This file is just a template!** Not
  actually loaded by live Emacs sessions -- it is copied to `~/.emacs.d/init.el`
  and then adjusted for each host.
- `early-init.el` — Startup tuning, loaded by Emacs before `init.el`, tweaks
  some values to speed up time-to-interactive. **This file is just a template!**
  Like `init.el`, actual loaded file is at `~/.emacs.d/early-init.el`.
- `config.el` — Main Emacs configuration, organized into sections by
  `use-package` declarations. Not byte-compiled; loaded from
  `~/.emacs.d/init.el`.
- `basic.el` — Personal convenience commands and helper functions (the `basic/`
  prefix), for example window movement and actions from isearch. Generally,
  preferences and settings that are dynamic and likely to change go into
  `config.el`, whereas utilities that are more stable and less likely to change
  go into this file. Byte compiled into `basic.elc`; loaded from `config.el`.
- `jinja2-plain.el`, `stringtemplate-plain.el`, `asdl-plain.el` — Standalone
  major modes that fontify only their own language constructs and leave the
  rest of the text plain. Byte compiled; loaded on demand from `config.el`.
- `bash.linux` — Bash configuration for Linux. It sets up Emacs and vterm
  integration and adds the private `bin` directories to `PATH`.
- `bin/install-jdtls.sh` — Installs the Eclipse JDT language server on Ubuntu and macOS.
- `bin/ff` — Opens files in the running Emacs if one exists, otherwise starts a
  new Emacs process and connects to it. Talks to Emacs either through
  `emacsclient` or, if inside ghostel/vterm, through the terminal protocol and
  the `basic/ff-cmd` wait protocol.
