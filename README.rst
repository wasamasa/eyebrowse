eyebrowse
=========

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
:fetcher git :url "https://bitbucket.org/wasamasa/eyebrowse"))`` and
you should be good to go.  Installation via MELPA will be supported
eventually, too.

Usage
-----

Use ``M-x eyebrowse-mode`` to enable ``eyebrowse`` interactively.  If
you want to enable it automatically on startup, add ``(eyebrowse-mode
t)`` to your init file (either ``~/.emacs`` or
``~/.emacs.d/init.el``).

The default key bindings are:

============== ================================
Key bind       Function
============== ================================
``C-c C-' <``  Switch to previous window config
``C-c C-' >``  Switch to next window config
``C-c C-' '``  Switch to last window config
``C-c C-' "``  Close current window config
``C-c C-' 0``  Switch to window config ``0``
\...           ...
``C-c C-' 9``  Switch to window config ``9``
============== ================================

Further Customization
---------------------

Use ``M-x customize-group RET eyebrowse`` for a list of customizable
options.  The more interesting ones would be
``eyebrowse-wrap-around-p`` and ``eyebrowse-switch-back-and-forth-p``
which affect both wrap around and lazier switching.

If you're not happy with the default keybindings, a riskier set can be
enabled additionally either by executing ``M-:
(eyebrowse-setup-opinionated-keys)`` interactively or inserting
``(eyebrowse-setup-opinionated-keys)`` in your init file.  If the
function detects the `evil <https://gitorious.org/evil>`_ package, it
will enable extra key bindings for it as well.

The extra key bindings are:

======== ================================
Key bind Function
======== ================================
``C-<``  Switch to previous window config
``gT``   ...
``C->``  Switch to next window config
``gt``   ...
``C-'``  Switch to last window config
``zx``   ...
``C-"``  Close current window config
``gc``   ...
``M-0``  Switch to window config ``0``
\...     ...
``M-9``  Switch to window config ``9``
======== ================================

Name
----

Actually, I wanted to name this mode "eyebrows" for no real reason,
but then a silly typo happened.  The typo stuck.  So did the new name.
