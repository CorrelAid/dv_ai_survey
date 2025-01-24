import json

def load_examples_from_json_wrapper(filepath, fc):
    with open(filepath, 'r') as f:
        data = json.load(f)
    examples = []
    for item in data:
        examples.append(fc(item))
    return examples