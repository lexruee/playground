from pyld import jsonld
import sys
import json
import pprint
pp = pprint.PrettyPrinter(indent=4)

if len(sys.argv) == 1:
    raise RuntimeError("Input file is not specified!")

with open(sys.argv[1]) as file:

    # read json file
    doc = file.read()

    # convert to a dictionary
    dic = json.loads(doc)

    # extract the context
    #context = dic['@context'] # can be a url or a dictionary

    #print context
    expanded_doc = jsonld.expand(dic)

    print(json.dumps(expanded_doc, indent=2))
