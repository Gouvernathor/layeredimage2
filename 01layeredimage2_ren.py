import renpy # type: ignore

python_object = object

"""renpy
init offset = -100

python early in layeredimage2:
"""

from types import MappingProxyType
from collections import defaultdict
from renpy.atl import parse_atl, RawBlock # type: ignore
from renpy.display.transform import ATLTransform # type: ignore
from store import At, ConditionSwitch, config, eval, Fixed, Null, Text, Transform # type: ignore

from store.layeredimage import format_function # type: ignore

EMPTYMAP = MappingProxyType({})

ATL_PROPERTIES = frozenset(renpy.atl.PROPERTIES)

# The properties for the Fixed wrapping the layeredimage
FIXED_PROPERTIES = frozenset(renpy.sl2.slproperties.position_property_names).union(renpy.sl2.slproperties.box_property_names)

# The properties taken at the base level of the layeredimage
BASE_PROPERTIES = ATL_PROPERTIES | FIXED_PROPERTIES | {"image_format", "format_function", "attribute_function", "offer_screen", "at"}
# The properties for all layers
LAYER_PROPERTIES = ATL_PROPERTIES | {"if_attr", "at"}
# The properties for attribute layers
ATTRIBUTE_PROPERTIES = LAYER_PROPERTIES | {"variant", "default"}
# The properties for the group statement
GROUP_BLOCK_PROPERTIES = LAYER_PROPERTIES | {"auto"}
GROUP_INLINE_PROPERTIES = GROUP_BLOCK_PROPERTIES | {"prefix", "variant"}
# The properties for the always layers
ALWAYS_PROPERTIES = LAYER_PROPERTIES
# The properties for the condition layers
CONDITION_PROPERTIES = LAYER_PROPERTIES

predict_all = None


def resolve_image(img):
    if img is None:
        return None
    if isinstance(img, RawBlock):
        return ATLTransform(img)
    else:
        return eval(img)

def resolve_at(at):
    """
    Turns an ATL RawBlock, or a Transform, or an iterable of Transforms,
    into a tuple of transforms.
    Passing RawBlocks to the callables is not documented, of course.
    """
    if isinstance(at, RawBlock):
        return (ATLTransform(at),)
    return renpy.easy.to_tuple(at)


class IfAttr(python_object):
    """
    Represents an if_attr expression.
    Abstract base class.
    """
    __slots__ = ()

    def __and__(self, other):
        return IfAnd(self, other)

    def __invert__(self): # only for test purposes
        return IfNot(self)

    def __or__(self, other): # only for test purposes
        return IfOr(self, other)

    @staticmethod
    def parse(l):
        l.require(r"\(", "parenthesized if_attr expression")
        rv = IfOr.parse(l)
        l.require(r"\)", "closing parenthesis1")
        return rv

    def __repr__(self): # only for test purposes
        t = type(self)
        return "".join((
            t.__name__,
            "(",
            ", ".join(repr(getattr(self, k)) for k in t.__slots__),
            ")",
        ))

class IfAttribute(IfAttr):
    __slots__ = ("attribute",)

    def __init__(self, attribute):
        self.attribute = attribute

    def check(self, attributes):
        return self.attribute in attributes

    @staticmethod
    def parse(l):
        if l.match(r"\("):
            rv = IfOr.parse(l)
            l.require(r"\)", "closing parenthesis2")
            return rv
        else:
            name = l.require(l.image_name_component, "attribute name")
            rv = IfAttribute(name)
            return rv

class IfNot(IfAttr):
    __slots__ = ("ifattr",)

    def __init__(self, ifattr):
        self.ifattr = ifattr

    def check(self, attributes):
        return not self.ifattr.check(attributes)

    @staticmethod
    def parse(l):
        if l.match("!") or l.keyword("not"):
            return IfNot(IfNot.parse(l))
        else:
            return IfAttribute.parse(l)

class IfAnd(IfAttr):
    __slots__ = ("first", "second")

    def __init__(self, first, second):
        self.first = first
        self.second = second

    def check(self, attributes):
        return self.first.check(attributes) and self.second.check(attributes)

    @staticmethod
    def parse(l):
        rv = IfNot.parse(l)
        while l.match("&") or l.keyword("and"):
            rv = IfAnd(rv, IfNot.parse(l))
        return rv

class IfOr(IfAttr):
    __slots__ = ("first", "second")

    def __init__(self, first, second):
        self.first = first
        self.second = second

    def check(self, attributes):
        return self.first.check(attributes) or self.second.check(attributes)

    @staticmethod
    def parse(l):
        rv = IfAnd.parse(l)
        while l.match(r"\|") or l.keyword("or"):
            rv = IfOr(rv, IfAnd.parse(l))
        return rv


class Layer(object):
    """
    Abstract base class for all the layers.
    """
    def __init__(self, if_attr=None, at=(), group_args=EMPTYMAP, **kwargs):
        self.at = resolve_at(at)
        self.if_attr = if_attr

        self.group_args = group_args
        self.transform_args = kwargs

    def check(self, attributes):
        """
        Wrapper for if_attr.check, allows self.if_attr to be None in most cases.
        """
        if self.if_attr is None:
            return True
        return self.if_attr.check(attributes)

    # same as the original
    def wrap(self, d):
        d = At(d, *self.at)

        if self.group_args or self.transform_args:
            d = Transform(d)

            for k, v in self.group_args.items():
                setattr(d, k, v)

            for k, v in self.transform_args.items():
                setattr(d, k, v)

        return d

    # abstract method
    def apply_format(self, li):
        """
        Actuates the displayable for this layer by passing infos to li's format_function.
        format_function keeps its legacy signature.
        """
        # Layeredimage.add calls layer.apply_format, passing itself by parameter
        # apply_format of the layers call format_function on the layeredimage
            # in the original it calls li.format which proxies to format_function but it's the same thing
        # format_function takes various information and returns a displayable-able
            # the default one always returns a string
        # the layer sets its own displayable to the result, after wrapping it using Layer.wrap
        raise NotImplementedError

    # abstract method
    def get_displayable(self, attributes):
        """
        Returns the displayable for this layer.
        """
        raise NotImplementedError

class Attribute(Layer):
    def __init__(self, group_name, attribute_name, displayable=None, default=False, *, prefix=None, variant=None, **kwargs):
        super().__init__(**kwargs)

        self.group_name = group_name
        self.raw_attribute_name = attribute_name

        if prefix is not None:
            attribute_name = "_".join((prefix, attribute_name))

        self.attribute_name = attribute_name
        self.displayable = displayable
        self.default = default
        self.variant = variant

    def apply_format(self, li):
        self.displayable = self.wrap(li.format_function(
            what="Attribute ({!r}, {!r})".format(self.group_name, self.attribute_name),
            name=li.name,
            group=self.group_name,
            variant=self.variant,
            attribute=self.raw_attribute_name,
            image=self.displayable,
            image_format=li.image_format,
        ))

    def get_displayable(self, attributes):

        if self.attribute_name not in attributes:
            return None

        if not self.check(attributes):
            return None

        return self.displayable

class Condition(Layer):
    def __init__(self, condition, displayable, **kwargs):
        super().__init__(**kwargs)

        self.condition = condition
        self.displayable = displayable

    def apply_format(self, li):
        self.displayable = self.wrap(li.format_function(
            what="Condition ({})".format(self.condition),
            name=li.name,
            group=None,
            variant=None,
            attribute=None,
            image=self.displayable,
            image_format=li.image_format,
        ))

    def get_displayable(self, attributes):

        if not self.check(attributes):
            return None

        return ConditionSwitch(
            self.condition, self.displayable,
            None, Null(),
            predict_all=predict_all,
        )

class ConditionGroup(Layer):
    def __init__(self, conditions):
        super().__init__()

        self.conditions = conditions

    # same as the original
    def apply_format(self, li):
        for i in self.conditions:
            i.apply_format(li)

    # same as the original
    def get_displayable(self, attributes):
        args = []

        for i in self.conditions:
            if not i.check(attributes):
                continue

            args.append(i.condition)
            args.append(i.displayable)

        args.append(None)
        args.append(Null())

        return ConditionSwitch(predict_all=predict_all, *args)

class Always(Layer):
    def __init__(self, displayable, **kwargs):
        super().__init__(**kwargs)

        self.displayable = displayable

    def apply_format(self, li):
        self.displayable = self.wrap(li.format_function(
            what="Always",
            name=li.name,
            group=None,
            variant=None,
            attribute=None,
            image=self.displayable,
            image_format=li.image_format,
        ))

    def get_displayable(self, attributes):

        if not self.check(attributes):
            return None

        return self.displayable

class LayeredImage(object):
    def __init__(self, layers, at=(), name=None, image_format=None, format_function=format_function, attribute_function=None, offer_screen=None, **kwargs):
        self.name = name
        self.image_format = image_format
        self.format_function = format_function
        self.attribute_function = attribute_function
        self.offer_screen = offer_screen

        self.attributes = [] # type: list[Attribute]
        self.layers = []

        self.attribute_to_groups = defaultdict(set) # type: dict[str, set[str]]
        self.group_to_attributes = defaultdict(set) # type: dict[str, set[str]]

        for i in layers:
            self.add(i)

        self.at = resolve_at(at)

        kwargs.setdefault("xfit", True)
        kwargs.setdefault("yfit", True)

        self.fixed_args = {k: kwargs.pop(k) for k in FIXED_PROPERTIES.intersection(kwargs)}
        self.transform_args = kwargs

    # the format method is forgotten, on purpose
    format = None

    # almost same as the original, except for variable and parameter names
    def add(self, layer):
        if not isinstance(layer, Layer):
            layer = Always(layer)

        layer.apply_format(self)
        self.layers.append(layer)

        if isinstance(layer, Attribute):
            self.attributes.append(layer)

            if layer.group_name is not None:
                self.attribute_to_groups[layer.attribute_name].add(layer.group_name)
                self.group_to_attributes[layer.group_name].add(layer.attribute_name)

    # almost same as the original, except for variable names
    def get_banned(self, attributes):
        banned = set()

        for a1 in attributes:
            for gn in self.attribute_to_groups[a1]:
                for a2 in self.group_to_attributes[gn]:
                    if a1 != a2:
                        banned.add(a2)

        return banned

    # same as the original
    def _duplicate(self, args):
        attributes = set(args.args)
        unknown = set(args.args)
        banned = self.get_banned(attributes)

        for a in self.attributes:

            unknown.discard(a.attribute_name)

            if a.default and (a.attribute_name not in banned):
                attributes.add(a.attribute_name)

        if self.attribute_function:
            attributes = set(self.attribute_function(attributes))

            unknown = {i[1:] if i.startswith('-') else i for i in attributes}

            for a in self.attributes:

                unknown.discard(a.attribute_name)

                if a.variant:
                    unknown.discard(a.variant)

        rv = Fixed(**self.fixed_args)

        offer_screen = self.offer_screen
        if offer_screen is None:
            offer_screen = config.layeredimage_offer_screen
        if offer_screen:
            rv._offer_size = (config.screen_width, config.screen_height)

        for i in self.layers:
            d = i.get_displayable(attributes)

            if d is not None:

                if d._duplicatable:
                    d = d._duplicate(None)

                rv.add(d)

        if unknown and args.lint:
            args = args.copy()
            args.args = tuple(unknown)
            args.extraneous()

        if unknown and config.developer:

            message = [" ".join(args.name), "unknown attributes:", " ".join(sorted(unknown))]

            text = Text(
                "\n".join(message),
                size=16,
                xalign=0.5,
                yalign=0.5,
                textalign=0.5,
                color="#fff",
                outlines=[ (1, "#0008", 0, 0) ],
            )

            rv = Fixed(rv, text, fit_first=True)

        rv = At(rv, *self.at)

        if self.transform_args:
            rv = Transform(child=rv, **self.transform_args)

        return rv

    # same as the original
    def _list_attributes(self, tag, attributes):

        banned = self.get_banned(attributes)

        ga_pairs = []

        seen = set()

        group_count = 0
        old_group = None

        for a in self.attributes:
            if a.group_name != old_group:
                old_group = a.group_name
                group_count += 1

            if a.attribute_name in banned:
                continue

            if a.attribute_name in seen:
                continue

            seen.add(a.attribute_name)
            ga_pairs.append((group_count, a.attribute_name))

        ga_pairs.sort()

        return [p[1] for p in ga_pairs]

    # same as the original
    def _choose_attributes(self, tag, required, optional):

        rv = list(required)

        required = set(required)
        banned = self.get_banned(required)
        both = required & banned

        if both:
            raise Exception("The specified attributes for {} are in conflict : {}".format(tag, ", ".join(both)))

        available_attributes = set(a.attribute_name for a in self.attributes)

        if optional is not None:
            optional = set(optional) & available_attributes
            rv.extend(optional - required - banned)

        if set(rv) - available_attributes:
            return None

        return tuple(rv)


def parse_property(l, final_properties, expr_properties, names):
    """
    Parses a property among the provided names and stores it
    inside the appropriate dict.
    Returns 0 if it didn't find any property,
    1 if it found a normal, inline property,
    and 2 if it found a block property (meaning "at transform").
    """

    check = l.checkpoint()

    name = l.word()

    if name not in names:
        l.revert(check)
        return 0

    if (name in final_properties) or (name in expr_properties):
        l.error("Duplicate property: {}".format(name))

    if name in ("auto", "default"):
        final_properties[name] = True
    elif name == "if_attr":
        final_properties[name] = IfAttr.parse(l)
    elif name in ("variant", "prefix"):
        final_properties[name] = l.require(l.image_name_component)
    elif name == "at":
        if l.keyword("transform"):
            l.require(":")
            l.expect_eol()
            l.expect_block("ATL")
            final_properties[name] = parse_atl(l.subblock_lexer())
            return 2
        else:
            expr_properties[name] = l.require(l.comma_expression)
    else:
        expr_properties[name] = l.require(l.simple_expression)

    return 1

def parse_displayable(l):
    """
    Parses either "image:" opening an ATL block and returns a RawBlock,
    or a simple expression and returns an evalable string,
    or returns None.
    """

    if l.keyword("image"):
        l.require(":")
        l.expect_eol()
        l.expect_block("ATL image")
        return parse_atl(l.subblock_lexer())
    else:
        return l.simple_expression()

class LayerNode(python_object):
    """
    An abstract base class which will probably be removed eventually,
    just to clarify the interface of the Node classes.
    """
    def __init__(self):
        self.final_properties = {}
        self.expr_properties = {}

    @staticmethod
    def parse(l):
        """
        Takes a lexer, and returns an instance of itself.
        """
        raise NotImplementedError

    def execute(self):
        """
        Returns a list of Layer instances.
        Usually of the referred type (AttributeNode -> Attribute), but not always.
        But they're always in a list.
        May take additional parameters, depending on the type of node.
        """
        raise NotImplementedError

class AttributeNode(LayerNode):
    def __init__(self, name):
        super().__init__()
        self.name = name
        self.displayable = None

    @staticmethod
    def parse(l):
        name = l.require(l.image_name_component)

        self = AttributeNode(name)

        got_block = False

        def line(lex):
            nonlocal got_block
            while True:
                pp = parse_property(lex, self.final_properties, self.expr_properties, ATTRIBUTE_PROPERTIES)
                if pp:
                    if pp == 2:
                        got_block = True
                        return
                    continue

                if lex.match("null"):
                    displayable = "Null()"
                else:
                    displayable = parse_displayable(lex)

                if displayable is not None:
                    if self.displayable is not None:
                        lex.error("An attribute can only have zero or one displayable, two found : {} and {}".format(displayable, self.displayable))

                    self.displayable = displayable

                    if isinstance(displayable, RawBlock):
                        got_block = True
                        return
                    continue

                break

        line(l)

        if got_block:
            return self
        elif not l.match(":"):
            l.expect_eol()
            l.expect_noblock("attribute")
            return self

        l.expect_block("attribute")
        l.expect_eol()

        ll = l.subblock_lexer()

        while ll.advance():
            if ll.keyword("pass"):
                ll.expect_eol()
                ll.expect_noblock("pass")
                continue

            line(ll)
            if got_block:
                got_block = False
            else:
                ll.expect_eol()
                ll.expect_noblock("attribute")

        if (self.displayable is not None) and ("variant" in self.final_properties):
            l.error("An attribute cannot have both a variant and a provided displayable")
        return self

    def execute(self, group_name=None, **group_properties):
        if "if_attr" in self.final_properties:
            if "if_attr" in group_properties:
                self.final_properties["if_attr"] &= group_properties.pop("if_attr")

        group_args = {k: group_properties.pop(k) for k in ATL_PROPERTIES.intersection(group_properties)}

        properties = (group_properties # the remaining ones, overwritten by the following
            | self.final_properties
            | {k: eval(v) for k, v in self.expr_properties.items()}
        )

        return [Attribute(group_name, self.name, resolve_image(self.displayable), group_args=group_args, **properties)]

class AttributeGroupNode(LayerNode):
    def __init__(self, li_name, group_name):
        super().__init__()
        self.li_name = li_name
        self.group_name = group_name
        self.children = []

    @staticmethod
    def parse(l, li_name):
        if l.keyword("multiple"):
            group_name = None
        else:
            group_name = l.require(l.image_name_component)

        self = AttributeGroupNode(li_name, group_name)

        got_block = False

        while True:
            pp = parse_property(l, self.final_properties, self.expr_properties, GROUP_INLINE_PROPERTIES)
            if pp == 1:
                continue
            elif pp == 2:
                got_block = True
            break

        if not got_block:
            if l.match(":"):
                l.expect_block("group")
                l.expect_eol()

                ll = l.subblock_lexer()

                while ll.advance():
                    got_block = False

                    if ll.keyword("pass"):
                        ll.expect_eol()
                        ll.expect_noblock("pass")
                        continue

                    if ll.keyword("attribute"):
                        attribute_node = AttributeNode.parse(ll)
                        # if "variant" in attribute_node.final_properties:
                        #     ll.error("Attribute {} cannot have a variant while in a group".format(attribute_node.name))
                        self.children.append(attribute_node)
                        continue

                    while True:
                        pp = parse_property(ll, self.final_properties, self.expr_properties, GROUP_BLOCK_PROPERTIES)
                        if pp == 1:
                            continue
                        elif pp == 2:
                            got_block = True
                        break

                    if got_block:
                        got_block = False
                    else:
                        ll.expect_eol()
                        ll.expect_noblock("group property")

            else:
                l.expect_eol()
                l.expect_noblock("group")

        if "variant" in self.final_properties:
            for an in self.children:
                if "variant" in an.final_properties:
                    l.error("Attribute {!r} has a variant, it cannot be in group {!r} which also has a variant".format(an.name, group_name))
        elif (group_name is None) and ("auto" in self.final_properties):
            l.error("A group without a variant cannot be multiple and auto at the same time")

        return self

    def execute(self):
        group_name = self.group_name

        properties = self.final_properties | {k: eval(v) for k, v in self.expr_properties.items()}

        auto = properties.pop("auto", False)
        variant = properties.get("variant", None)

        rv = []

        for an in self.children:
            rv.extend(an.execute(group_name=group_name, **properties))

        if auto:
            seen = set(a.raw_attribute_name for a in rv)
            pattern = format_function(
                what="auto group attribute",
                name=self.li_name.replace(" ", "_"),
                group=group_name or None,
                variant=variant or None,
                attribute="",
                image=None,
                image_format=None,
            )

            for i in renpy.list_images():
                if i.startswith(pattern):
                    attr, *s = i.removeprefix(pattern).split()
                    if (not s) and (attr not in seen):
                        rv.append(Attribute(group_name, attr, renpy.displayable(i), **properties))

        return rv

class ConditionNode(LayerNode):
    def __init__(self, condition):
        super().__init__()
        self.condition = condition
        self.displayable = None

    @staticmethod
    def parse(l, need_expr):
        l.skip_whitespace()

        if need_expr:
            condition = l.require(l.comma_expression)
        else:
            condition = None
        self = ConditionNode(condition)

        l.require(":")
        l.expect_block("if/elif/else")
        l.expect_eol()

        ll = l.subblock_lexer()

        while ll.advance():
            # not useful because an if/elif/else needs a displayable so it can't be empty
            # if ll.keyword("pass"):
            #     ll.expect_eol()
            #     ll.expect_noblock("pass")
            #     continue

            got_block = False

            while True:
                pp = parse_property(ll, self.final_properties, self.expr_properties, CONDITION_PROPERTIES)
                if pp:
                    if pp == 2:
                        got_block = True
                        break
                    continue

                displayable = parse_displayable(ll)
                if displayable is not None:
                    if self.displayable is not None:
                        ll.error("An if, elif or else statement can only have one displayable, two found : {} and {}".format(displayable, self.displayable))

                    self.displayable = displayable

                    if isinstance(displayable, RawBlock):
                        got_block = True
                        break
                    continue

                break

            if not got_block:
                ll.expect_noblock("if/elif/else properties")
                ll.expect_eol()

        if self.displayable is None:
            ll.error("An if, elif or else statement must have a displayable")

        return self

    def execute(self):
        properties = self.final_properties | {k: eval(v) for k, v in self.expr_properties.items()}
        return [Condition(self.condition, resolve_image(self.displayable), **properties)]

class ConditionGroupNode(LayerNode):
    def __init__(self, conditions=()):
        # no super, on purpose
        self.conditions = conditions

    @staticmethod
    def parse(l):
        conditions = []

        conditions.append(ConditionNode.parse(l, True))
        l.advance()

        while l.keyword("elif"):
            conditions.append(ConditionNode.parse(l, True))
            l.advance()

        if l.keyword("else"):
            conditions.append(ConditionNode.parse(l, False))
        else:
            l.unadvance()

        return ConditionGroupNode(conditions)

    def execute(self):
        l = []
        for c in self.conditions:
            l.extend(c.execute())

        return [ConditionGroup(l)]

class AlwaysNode(LayerNode):
    def __init__(self):
        super().__init__()
        self.displayable = None

    @staticmethod
    def parse(l):
        self = AlwaysNode()

        got_block = False

        def line(lex):
            nonlocal got_block
            while True:
                pp = parse_property(lex, self.final_properties, self.expr_properties, ALWAYS_PROPERTIES)
                if pp:
                    if pp == 2:
                        got_block = True
                        return
                    continue

                displayable = parse_displayable(lex)

                if displayable is not None:
                    if self.displayable is not None:
                        lex.error("The always statement can only have one displayable, two found : {} and {}".format(displayable, self.displayable))

                    self.displayable = displayable

                    if isinstance(displayable, RawBlock):
                        got_block = True
                        return
                    continue

                break

        line(l)

        if got_block:
            return self
        elif not l.match(":"):
            l.expect_eol()
            l.expect_noblock("always")
            return self

        l.expect_block("always")
        l.expect_eol()

        ll = l.subblock_lexer()

        while ll.advance():
            # useful since an always can have its child inline so it can have an empty block
            if ll.keyword("pass"):
                ll.expect_eol()
                ll.expect_noblock("pass")
                continue

            line(ll)
            if got_block:
                got_block = False
            else:
                ll.expect_eol()
                ll.expect_noblock("always")

        if self.displayable is None:
            l.error("The always statement must have a displayable")

        return self

    def execute(self):
        properties = self.final_properties | {k: eval(v) for k, v in self.expr_properties.items()}
        return [Always(resolve_image(self.displayable), **properties)]

class LayeredImageNode(python_object):
    def __init__(self, name):
        self.name = name
        self.children = []
        self.final_properties = {}
        self.expr_properties = {}

    @staticmethod
    def parse(l):
        name = [l.require(l.image_name_component)]

        part = l.image_name_component()
        while part is not None:
            name.append(part)
            part = l.image_name_component()

        name = " ".join(name)

        l.require(":")
        l.expect_block("layeredimage2")

        ll = l.subblock_lexer()
        ll.advance()

        self = LayeredImageNode(name)

        while not ll.eob:
            if ll.keyword("attribute"):
                self.children.append(AttributeNode.parse(ll))

            elif ll.keyword("group"):
                self.children.append(AttributeGroupNode.parse(ll, name))

            elif ll.keyword("if"):
                self.children.append(ConditionGroupNode.parse(ll))

            elif ll.keyword("always"):
                self.children.append(AlwaysNode.parse(ll))

            elif ll.keyword("pass"):
                ll.expect_eol()
                ll.expect_noblock("pass")

            else:
                pp = 1
                while pp == 1:
                    pp = parse_property(ll, self.final_properties, self.expr_properties, BASE_PROPERTIES)

                if not pp:
                    ll.expect_noblock("layeredimage property")
                    ll.expect_eol()

            ll.advance()

        return self

    def execute(self):
        properties = self.final_properties | {k: eval(v) for k, v in self.expr_properties.items()}

        l = []
        for i in self.children:
            l.extend(i.execute())

        renpy.image(
            self.name,
            LayeredImage(
                l,
                name=self.name.replace(" ", "_"),
                **properties,
            )
        )


renpy.register_statement("layeredimage2", parse=LayeredImageNode.parse, execute=LayeredImageNode.execute, init=True, block=True)
