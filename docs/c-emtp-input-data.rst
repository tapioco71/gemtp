.. comment: -*- mode:rst; coding:utf-8; electric-indent-mode:nil; tab-always-indent:t -*-


Address
================================================================================

An `Address` instance represent a "postal" address, a mean to identify a
geographic location.

There are mappings between `Coordinates`\'s and `Address`\es.


Responsibilities
--------------------------------------------------------------------------------

- Record a postal address.
- Find the corresponding coordinates.


Attributes
--------------------------------------------------------------------------------

indications
    ``string`` an optional free-form text indicating how to find the geographic
    location (appartment numbers, door codes, path, etc).

lines
    ``string`` a multi-line text containing the postal address.

code
    ``string`` a string containing the postal code.

city
    ``string`` the name of the city.

country
    ``string`` the name of the country.


Associations
--------------------------------------------------------------------------------

- An `Address` is associated to 0 or 1 `Coordinates` object containing the
  geographic coordinates of the address.


References
--------------------------------------------------------------------------------

- `<https://en.wikipedia.org/wiki/Address_%28geography%29>`_ (have fun!)
- `<https://www.mjt.me.uk/posts/falsehoods-programmers-believe-about-addresses/>`_
- `<http://wiesmann.codiferes.net/wordpress/?p=15187&lang=en>`_

.. comment: the end
