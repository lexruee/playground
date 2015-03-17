from pyld import jsonld
import json

doc = {
    "http://schema.org/name": "Manu Sporny",
    "http://schema.org/url": {"@id": "http://manu.sporny.org/"},
    "http://schema.org/image": {"@id": "http://manu.sporny.org/images/manu.png"}
}

context = {
    "name": "http://schema.org/name",
    "homepage": {"@id": "http://schema.org/url", "@type": "@id"},
    "image": {"@id": "http://schema.org/image", "@type": "@id"}}

# compact a document according to a particular context
# see: http://json-ld.org/spec/latest/json-ld/#compacted-document-form
compacted = jsonld.compact(doc, context)

print(json.dumps(compacted, indent=2))
# Output:
# {
#   "@context": {...},
#   "image": "http://manu.sporny.org/images/manu.png",
#   "homepage": "http://manu.sporny.org/",
#   "name": "Manu Sporny"
# }


# expand a document, removing its context
# see: http://json-ld.org/spec/latest/json-ld/#expanded-document-form
expanded = jsonld.expand(compacted)

print(json.dumps(expanded, indent=2))
