eyebrowse-mode
==============

.. image:: http://bitbucket.org/wasamasa/eyebrowse/raw/HEAD/eyebrows.gif

About
-----

``eyebrowse`` is a global minor mode for Emacs that allows you to manage
your window configurations in a simple manner.  It displays their
current state in the modeline by default.  The behaviour is modeled
after `ranger <http://ranger.nongnu.org/>`_, a file manager written in
Python.

Installation
------------

For now installation is only supported via `quelpa
<https://github.com/quelpa/quelpa>`_.  Use ``M-: (quelpa '(eyebrowse
:fetcher bitbucket :repo "wasamasa/eyebrowse"))`` and you should be
good to go.  Installation via MELPA will be supported eventually, too.

Usage
-----

Use ``M-x eyebrowse-mode`` to enable ``eyebrowse`` interactively.  If
you want to enable it automatically on startup, add ``(eyebrowse-mode
t)`` to your init file (either ``~/.emacs`` or
``~/.emacs.d/init.el``).

The default key bindings are:

============= ================================
Key bind      Function
============= ================================
``C-c C-' <`` Switch to previous window config
``C-c C-' >`` Switch to next window config
``C-c C-' \`
``            Close current window config
``C-c C-' '`` Switch to last window config
``C-c C-' 0`` Switch to window config ``0``
\...          ...
``C-c C-' 9`` Switch to window config ``9``
============= ================================

Further Customization
---------------------

Name
----

Actually, I wanted to name this mode "eyebrows" for no real reason,
but then a silly typo happened.  The typo stuck.  So did the new name.
