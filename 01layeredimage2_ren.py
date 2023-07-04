import renpy # type: ignore
import store # type: ignore

python_object = object

"""renpy
init offset = -100

python early in layeredimage2:
"""

from types import MappingProxyType
from collections import defaultdict
# from store import Transform, ConditionSwitch, Fixed, Null, config, Text, eval, At
from store import At, ConditionSwitch, config, Fixed, Null, Text, Transform # type: ignore

from store.layeredimage import format_function # type: ignore

EMPTYMAP = MappingProxyType({})

ATL_PROPERTIES = frozenset(i for i in renpy.atl.PROPERTIES)

# The properties for the Fixed wrapping the layeredimage
FIXED_PROPERTIES = frozenset(i for i in renpy.sl2.slproperties.position_property_names).union(renpy.sl2.slproperties.box_property_names)

# The properties taken at the base level of the layeredimage
BASE_PROPERTIES = ATL_PROPERTIES | FIXED_PROPERTIES | {"image_format", "format_function", "attribute_function", "offer_screen", "at"}
# The properties for all layers
LAYER_PROPERTIES = ATL_PROPERTIES | {"if_attr", "at"}
# The properties for attribute layers
ATTRIBUTE_PROPERTIES = LAYER_PROPERTIES | {"variant", "default"}
# The properties for the group statement
GROUP_PROPERTIES = LAYER_PROPERTIES # | {"auto", "variant", "prefix"}
# The properties for the always layers
ALWAYS_PROPERTIES = LAYER_PROPERTIES
# The properties for the condition layers
CONDITION_PROPERTIES = LAYER_PROPERTIES

# The properties whose result should not be evaluated, from the Node to the Layer
VERBATIM_PROPERTIES = frozenset(("if_attr", "variant", "prefix", "default", "auto", "multiple"))

predict_all = None


class IfAttr(python_object):
    """
    Represents an if_attr expression.
    """
    __slots__ = ()

    def __init__(self):
        raise NotImplementedError

    def check(self, attributes):
        raise NotImplementedError


class Layer(object):
    """
    Abstract base class for all the layers.
    """
    def __init__(self, if_attr=None, at=(), group_args=EMPTYMAP, **kwargs):
        self.at = renpy.easy.to_tuple(at)
        self.if_attr = if_attr

        self.group_args = group_args
        self.transform_args = kwargs

    def check(self, attributes):
        """
        Wrapper for if_attr.check, allows if_attr to be None in most cases.
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
            "Attribute ({!r}, {!r})".format(self.group_name, self.attribute_name),
            group=self.group_name,
            variant=self.variant,
            attribute=self.raw_attribute_name,
            image=self.displayable,
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
            "Condition ({})".format(self.condition),
            group=None,
            attribute=None,
            image=self.displayable,
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
            args.append(i.image)

        args.append(None)
        args.append(Null())

        return ConditionSwitch(predict_all=predict_all, *args)

class Always(Layer):
    def __init__(self, displayable, **kwargs):
        super().__init__(**kwargs)

        self.displayable = displayable

    def apply_format(self, li):
        self.displayable = self.wrap(li.format_function(
            "Always",
            group=None,
            attribute=None,
            image=self.displayable,
        ))

    def get_displayable(self, attributes):

        if not self.check(attributes):
            return None

        return self.displayable

class LayeredImage(object):
# class LayeredImage(store.layeredimage.LayeredImage):
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

        self.at = renpy.easy.to_tuple(at)

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
                    banned.add(a2)

        return banned

    # same as the original
    def _duplicate(self, args):

            name = " ".join(args.name + tuple(args.args))

            attributes = set(args.args)
            unknown = set(args.args)
            banned = self.get_banned(attributes)

            for a in self.attributes:

                unknown.discard(a.attribute_name)

                if a.default and (a.attribute_name not in banned):
                    attributes.add(a.attribute_name)

            if self.attribute_function:
                attributes = set(self.attribute_function(attributes))

                unknown = set([i[1:] if i.startswith('-') else i for i in attributes])

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


class LayerNode(python_object):
    """
    An abstract base class which will probably be removed eventually,
    just to clarify the interface of the Node classes.
    """
    def __init__(self):
        raise NotImplementedError

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
        self.name = name
        self.displayable = None
        self.final_properties = {}
        self.expr_properties = {}

    @staticmethod
    def parse(l):
        name = l.require(l.image_name_component)

        self = AttributeNode(name)

        def line(lex):
            while True:
                if parse_property(lex, self.final_properties, self.expr_properties, ATTRIBUTE_PROPERTIES):
                    continue

                if lex.match("null"):
                    displayable = "Null()"
                else:
                    displayable = lex.simple_expression()

                if displayable is not None:

                    if self.displayable is not None:
                        lex.error("An attribute can only have zero or one displayable, two found : {} and {}".format(displayable, self.displayable))

                    self.displayable = displayable
                    continue

                break

        line(l)

        if not l.match(":"):
            l.expect_eol()
            l.expect_noblock("attribute")
            return self

        l.expect_block("attribute")
        l.expect_eol()

        ll = l.subblock_lexer()

        while ll.advance():
            line(ll)
            ll.expect_eol()
            ll.expect_noblock("attribute")

        return self

class AttributeGroupNode(LayerNode):
    pass

class ConditionNode(LayerNode):
    pass

class ConditionGroupNode(LayerNode):
    pass

class AlwaysNode(LayerNode):
    pass

# almost same as original, but takes and mutates a dict
def parse_property(l, final_properties, expr_properties, names):
    """
    Parses a property among the provided names and stores the
    evalable value inside the properties dict.
    Returns True if it found a property, False if it didn't.
    """

    check = l.checkpoint()

    name = l.word()

    if name not in names:
        l.revert(check)
        return False

    if (name in final_properties) or (name in expr_properties):
        l.error("Duplicate property: {}".format(name))

    if name in ("auto", "default"):
        final_properties[name] = True
    elif name in ("variant", "prefix"):
        final_properties[name] = l.require(l.image_name_component)
    elif name == "at":
        expr_properties[name] = l.require(l.comma_expression)
    else:
        expr_properties[name] = l.require(l.simple_expression)

    return True

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
                ll.advance()

            elif ll.keyword("group"):
                self.children.append(AttributeGroupNode.parse(ll))
                ll.advance()

            elif ll.keyword("if"):
                self.children.append(ConditionGroupNode.parse(ll))
                # advances by itself # fix that ?

            elif ll.keyword("always"):
                self.children.append(AlwaysNode.parse(ll))
                ll.advance()

            else:
                while parse_property(ll, self.final_properties, self.expr_properties, BASE_PROPERTIES):
                    pass

                ll.expect_noblock("layeredimage element")
                ll.expect_eol()
                ll.advance()

        return self

    def execute(self):
        raise NotImplementedError


renpy.register_statement("layeredimage2", parse=LayeredImageNode.parse, execute=LayeredImageNode.execute, init=True, block=True)
