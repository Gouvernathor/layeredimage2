This code implements a new syntax for layeredimages.

::

    layeredimage2 new_way:
        attribute towel variant back
        group pose auto prefix pose
        group wear if_attr (!pose_all_inclusive):
            attribute tux
            attribute swimsuit if_attr (!(pose_swindling|pose_knitting|pose_swatting))
            attribute swimsuit variant swindling if_attr (pose_swindling)
            attribute swimsuit variant knitting if_attr (pose_knitting)
            attribute swimsuit variant swatting if_attr (pose_swatting)
        attribute towel if_attr (swimsuit|!pose_swindling)

Changes in a nutshell
---------------------

#. prefix and variant take a name without quotes
#. multiple groups don't take a name - not required anymore, and not even accepted anymore
#. auto multiple groups (groups which are both auto and multiple at the same time) change in behavior slightly by never using the group name at all (unlikely to have any real-life consequence, but the new behavior is more consistent)
#. attributes can take a variant, if they don't have an explicit displayable and if they're not in a group with a variant
#. if_any, if_all and if_not are gone, and replaced with if_attr and a dedicated, shorter syntax which does away with the quotes and allows a greater variety of conditions to be expressed
#. you can add pass to any block (layeredimage, always, group, attribute), except if/elif/else blocks which can't be empty in the first place
#. you can use the ``at transform:`` syntax with an ATL block like in screen language, in any place where you could use the ``at`` property, and you can use the similar ``image:`` syntax with an ATL block in any place requiring a displayable : inside if/elif/else blocks, and inline and in the blocks of always and attribute statements

Motivation and issues with the legacy syntax
--------------------------------------------

- The ``prefix`` and ``variant`` properties require quotes, because they can take arbitrary python expressions. But these expressions are evaluated at init time, so there's no real way of making use of that : you would need a value which changes between boots of the game, but not during gameplay ? How does that make sense.

  - now, these take a name - more specifically an image name component, which is basically a python identifier but can start with a digit - so they can do away with the quotes, which makes the script more readable

- Non-``auto`` ``multiple`` groups require a name to be passed, but that name is ignored. This is as weird as if some python statement forced you to write a comment and then, by purpose of it being a comment, didn't read it.

  - Now, ``multiple`` works by passing it as the group name : you write it ``group multiple ...``, with potentially other properties or a colon afterwards. That group *is* multiple, and it actually has no name.

- Attribute definition in ``multiple`` groups behave differently between the ``auto`` behavior and the auto attribution of displayables to non-auto attributes. Yes, that's a mouthful.

  Auto attributes match an image with the group's name in it, but the attributes written explicitly match an image not including the group's name

  For example in the following code::

    layeredimage eileen:
        group token multiple auto:
            attribute blue

  A potential "red" attribute would be defined by the ``auto`` behavior if it finds an "eileen_token_red" image, and that image would be attached to it.

  However, the non-auto "blue" attribute would be matched with the "eileen_blue" image, not "eileen_token_blue".

  As a result, if an "eileen_token_blue" image is defined, it would only be part of the layeredimage if ``attribute short`` was not written in the group. Toggling between "eileen_blue" and "eileen_token_blue" is very weird.

  - Now, both auto and non-auto attributes follow the same rules in ``multiple`` groups, since these groups don't have a name anymore.

    The former behavior (of auto-defining a bunch of non-mutually-exclusive attributes from images beginning the same way) can now be done using a ``variant``.

    The ``auto`` behavior still disregards the format_function passed to the layeredimage, but now at least it works the same way as the default format_function which treats ``multiple`` groups as being nameless.

- There is no way to provide a ``variant`` to a single attribute outside of a group.

  For example of when that would be useful, imagine that you have an element (a scarf for example) that is either present or not present but has no incompatibility with anything else (aka : you wouldn't intuitively put it in a group), and which is partly beneath, and partly over one or several other attributes::

      layeredimage eileen:
          attribute scarf # back part
          group arms auto
          attribute scarf # front part

  You could manually provide the images the scarf attributes use, but I find that tedious and tend to avoid it whenever possible, as it makes the code twice longer and it becomes quickly unreadable. Let's not do that.

  You can't name the back part ``attribute scarf_back``, because then the front part could appear without the back part showing, and vice versa. You could solve that with an ``adjust_attribute`` function, but let's avoid resorting to Python if it can be avoided. But if both attributes are named "scarf", they will both be matched to the same image.

  A workaround is to use a single-use ``multiple`` group::

      layeredimage eileen:
          group _ multiple variant "back":
              attribute scarf
          group arms auto
          attribute scarf

  Now, the "eileen_back_scarf.png" image is matched to the first attribute, and "eileen_scarf.png" to the second one. Both will be displayed when calling ``show eileen scarf``, with the arms in the middle.

  But using a multiple group that way makes a weird syntax.

  - Now, you can simply write it like this::

        layeredimage eileen:
            attribute scarf variant "back"
            group arms auto
            attribute scarf

    The ``variant`` property is however forbidden on attributes inside groups that already have a variant.

    It is also forbidden in attributes that are given an explicit displayable (because in that case the variant would have no effect).

- ``if_any``, ``if_all`` and ``if_not`` (communally the ``if_`` properties) are very handy.

  However, a combination of these properties only covers conditionals which can be expressed as ``any(...) and all(...) and not any(...)``.

  For example, ``(a or b) and (c or d)`` or ``not (a and b)`` cannot be expressed that way.

  Furthermore, you could give a condition to a group, and see one attribute not respect that condition. That's because when the same ``if_`` property is given to an attribute and to its group, the attribute's property *replaces* the group's rather than merging with it. That was issue #3955 on Ren'Py.

  - Now, the ``if_attr`` property offers a lot more freedom, avoids having to use quotes for the attributes, supports attribute-and-group ifs by using an ``and`` operator to combine them (that's because the ``if_`` property makes things under it appear less often, so when you add another ``if_``, you make it appear even less often), and may even allow including groups in the conditionals (that's not yet implemented).

How to convert to the new syntax
--------------------------------

Remove quotes from ``variant`` and ``prefix`` clauses. If that doesn't parse, you were doing something bad.

Remove the name in ``multiple`` groups : ``group a multiple`` -> ``group multiple``. If the group is also ``auto``:

- For auto multiple groups without a variant, add the former group name as a variant : ``group a auto multiple`` -> ``group multiple auto variant a``
- For auto multiple groups with a variant, add the former group name to the variant with an underscore : ``group a auto multiple variant "b"`` -> ``group multiple auto variant a_b``
- For auto multiple groups with attributes declared inside them, write the line just above on its own, and then add another multiple group with no variant (or with the former variant), with the same properties except that it is not auto, and put the attributes inside. Yes, it's complex, but you were doing something weird anyway.

Rephrase the ``if_`` properties using the new ``if_attr`` : ``if_any ["a", "b"] if_all ["c", "d"] if_not ["e", "f"]`` -> ``if_attr ((a | b) & c & d & !(e | f))``
