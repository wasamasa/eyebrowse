eyebrowse
=========

.. image:: https://raw.github.com/wasamasa/eyebrowse/master/img/eyebrows.gif

About
-----

``eyebrowse`` is a global minor mode for Emacs that allows you to
manage your window configurations in a simple manner, just like tiling
window managers like i3wm with their workspaces do.  It displays their
current state in the modeline by default.  The behaviour is modeled
after `ranger <http://ranger.nongnu.org/>`_, a file manager written in
Python.

Screenshot
----------

.. image:: https://raw.github.com/wasamasa/eyebrowse/master/img/scrot.png

See the lighter and the modeline indicator at the right side of the
bottom modeline?  That's what you get to see after enabling eyebrowse.

Installation
------------

Install via `quelpa <https://github.com/quelpa/quelpa>`_ with ``M-:
(quelpa 'eyebrowse)`` or install the package from `MELPA
<http://melpa.milkbox.net/>`_.

Quick Tutorial
--------------

Use ``M-x eyebrowse-mode`` to enable ``eyebrowse`` interactively.  If
you want to enable it automatically on startup, add ``(eyebrowse-mode
t)`` to your init file (either ``~/.emacs`` or
``~/.emacs.d/init.el``).

You start with your current window config on slot 1.  Once you hit
``C-c C-w 2``, you will see the modeline indicator appearing and
showing slot 1 and 2 with slot 2 slightly emphasized.  Slot 1 has been
saved automatically for you and contains your last window config.  Do
something meaningful like a window split, then hit ``C-c C-w 1``.  The
window config on slot 2 is saved and the window config from slot 1 is
loaded.  Try switching back and forth between them with ``C-c C-w '``
to get a feeling for how subsequent window manipulations are handled.

Key bindings
------------

The default key bindings are:

============== ================================
Key bind       Function
============== ================================
``C-c C-w <``  Switch to previous window config
``C-c C-w >``  Switch to next window config
``C-c C-w '``  Switch to last window config
``C-c C-w "``  Close current window config
``C-c C-w 0``  Switch to window config ``0``
\...           ...
``C-c C-w 9``  Switch to window config ``9``
============== ================================

Further Customization
---------------------

Use ``M-x customize-group RET eyebrowse`` for a list of customizable
options.  The more interesting ones would be
``eyebrowse-wrap-around-p`` and ``eyebrowse-switch-back-and-forth-p``
which affect both wrap around and lazier switching.

The prefix for each binding defaults to ``C-c C-w``, but you can change
it to something else by customizing ``eyebrowse-keymap-prefix``.  If
you want to change it in your init file, insert the customization
before enabling ``eyebrowse-mode``.

If you're not happy with the default keybindings, a riskier set can be
enabled additionally either by executing ``M-:
(eyebrowse-setup-opinionated-keys)`` interactively or inserting
``(eyebrowse-setup-opinionated-keys)`` in your init file.  If the
function detects the `evil <https://gitorious.org/evil>`_ package, it
will enable extra key bindings for it as well.

The extra key bindings are:

=============== ================================
Key bind        Function
=============== ================================
``C-<``, ``gT`` Switch to previous window config
``C->``, ``gt`` Switch to next window config
``C-'``, ``zx`` Switch to last window config
``C-"``, ``gc`` Close current window config
``M-0``         Switch to window config ``0``
\...            ...
``M-9``         Switch to window config ``9``
=============== ================================

Internals
---------

This mode basically wraps what ``C-x r w`` and ``C-x r j`` do.  The
difference is first, it saves and loads automatically for you upon
switching slots, and second, it doesn't overwrite the general purpose
registers.  What it does instead is keeping its own data structure (a
list of lists containing slot, window config and point) and using it
to provide some other convenience keybinds, such as jumping to the
last window config or the next one available.

Contributing
------------

If you find bugs, have suggestions or any other problems, feel free to
report an issue on the issue tracker or hit me up on IRC, I'm always on
``#emacs``.  Patches are welcome, too, just fork, work on a separate
branch and open a pull request with it.

List of contributors:

`See here <https://github.com/wasamasa/eyebrowse/graphs/contributors>`_

Changelog
---------

`See here <https://github.com/wasamasa/eyebrowse/commits/master>`_

Alternatives
------------

The two most popular window configuration packages are `elscreen
<https://github.com/shosti/elscreen>`_ and `escreen
<https://github.com/emacsmirror/escreen>`_.  Both are fairly old and
have their share of bugs.  If you want something more recent with more
features than eyebrowse provides (such as persistency, morphing,
per-view buffers, \...), try `workgroups
<https://github.com/tlh/workgroups.el>`_ or `workgroups2
<https://github.com/pashinin/workgroups2>`_.

Name
----

Actually, I wanted to name this mode "eyebrows" for no real reason,
but then a silly typo happened.  The typo stuck.  So did the new name.
