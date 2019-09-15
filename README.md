# Erlxml

[![Hex pm](https://img.shields.io/hexpm/v/erlxml.svg?style=flat-square&labelColor=5c676d&color=714a94)](https://hex.pm/packages/erlxml)
[![Build Status](https://secure.travis-ci.org/eyedouble/erlxml.svg?branch=master
"Build Status")](https://secure.travis-ci.org/eyedouble/erlxml)
[![License](https://img.shields.io/github/license/eyedouble/erlxml?color=007ec6&style=flat-square)](LICENSE)



**Modern Erlang / Elixir library for XML <-> map serialisation and deserialisation using XSD schema powered by OTP/xmerl** 

- [Install from hex.pm](https://hex.pm/packages/erlxml)
- [Documentation available on hexdoc](https://hexdocs.pm/erlxml)

__Version:__ 1.0.0

## Summary

Erlxml provides more modern approach for working with data coming from XML sources.
In order for Erlxml to determine how to deserialise XML a XML-schema also known as 
XSD must be provided.

Erlxml will validate against the provided XSD on serialisation and deserialisation.
This includes extensive type checks - Validation is done by xmerl_xsd module.

Type casting of data is will be attempted altough only a limited set of 
XML types is supported at this stage:
- `xs:string` <> `binary`
- `xs:decimal` <> `float`
- `xs:float` <> `float`
- `xs:int` <> `integer`
- `xs:integer` <> `integer`
- `xs:positiveInteger` <> `integer`
- `xs:negativeInteger` <> `integer`
- `xs:nonPositiveInteger` <> `integer`
- `xs:nonNegativeInteger` <> `integer`
- `xs:date` <> `{YYYY, MM, DD}` (Only "YYYY-MM-DD" format is supported at this stage)
- `xs:boolean` <> `boolean` or `0/1`

These and all other types are validated by the `xmerl_xsd` module, Erlxml handles unkown types 
gracefully by casting them to a binary string.

### About maps and lists
Erlxml deserialises XML data into maps. It uses the XML-schema (XSD) to determine if
a `tag` contains a list of `tags` or just some properties - Eg. books -> list of books vs book -> title, description etc.  Consider:

```
   % List element
   #{books=> [
   #{book => #{title => <<"Title">>, desc => <<"Desc">>}}
   ]}
   % Non-list element
   #{book => #{title => <<"Title">>, desc => <<"Desc">>}}
```

To determine if a `tag` should be deserialised as a 'list-element' Erlxml uses 
the `maxOccurs` attribute in your XML-schema (XSD). All elements containing an element with a `maxOccurs` attribute with a value other than `1` are considered 'list-elements'. 




## Usage
### Serialise
To be done

### Deserialise
- Build a Schema state by using `{ok, State} = erlxml:build_schema_state({file, "path/to/example/file.xsd"})`.
- Call `erlxml:deserialise("path/to/xml/file.xml, State)`.



## Contribute

For issues, comments or feedback please [create an
issue](http://github.com/eyedouble/erlxml/issues).

