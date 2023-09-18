import heapq

class Rule:
    """
    Class representing a grammar rule with its probability.
    """
    def __init__(self, lhs, rhs, prob):
        self.lhs = lhs  # Left-hand side symbol
        self.rhs = rhs  # Right-hand side symbols
        self.prob = prob  # Probability of the rule

class Item:
    """
    Class representing an item in the chart.
    """
    def __init__(self, symbol, start, end, prob, backpointer):
        self.symbol = symbol  # The symbol this item represents
        self.start = start  # Start position in the sentence
        self.end = end  # End position in the sentence
        self.prob = prob  # Probability of this item
        self.backpointer = backpointer  # Items that combine to form this item

    def __lt__(self, other):
        # Items are compared based on their probability for priority queue
        return self.prob > other.prob

def nbest_CKY(sentence, grammar, n):
    """
    The n-best version of CKY algorithm.
    """
    N = len(sentence)
    # Initialize the chart
    chart = [[[] for _ in range(N+1)] for _ in range(N+1)]

    # Initialization
    for i, word in enumerate(sentence):
        for rule in grammar:
            # If the rule is a terminal rule that matches the word
            if len(rule.rhs) == 1 and rule.rhs[0] == word:
                # Create a terminal item for word
                word_item = Item(word, i, i+1, 1, None)
                # Create an item for rule with word_item as backpointer
                rule_item = Item(rule.lhs, i, i+1, rule.prob, [word_item])
                heapq.heappush(chart[i][i+1], rule_item)

    # Algorithm
    for length in range(2, N+1):
        for start in range(N+1-length):
            end = start + length
            for split in range(start+1, end):
                for rule in grammar:
                    # If the rule is a binary rule
                    if len(rule.rhs) == 2:
                        B, C = rule.rhs
                        for b in chart[start][split]:
                            if b.symbol == B:
                                for c in chart[split][end]:
                                    if c.symbol == C:
                                        prob = rule.prob * b.prob * c.prob
                                        backpointer = [b, c]
                                        item = Item(rule.lhs, start, end, prob, backpointer)
                                        # If there are less than n items in the cell
                                        if len(chart[start][end]) < n:
                                            heapq.heappush(chart[start][end], item)
                                        # If this item has higher probability than the smallest item in the cell
                                        elif chart[start][end][0].prob < prob:
                                            heapq.heapreplace(chart[start][end], item)

    # Extract n-best parses
    parses = []
    while chart[0][N]:
        item = heapq.heappop(chart[0][N])
        parse = extract_parse(item)
        parses.append((item.prob, parse))

    if not parses:
        print("No parse found for the given sentence with the given grammar.")
        return None

    return parses

def extract_parse(item):
    """
    Recursively extract parse tree from an item.
    """
    if item.backpointer is None:  # Terminal item
        return item.symbol
    elif len(item.backpointer) == 1:  # Unary rule
        return "(" + item.symbol + " " + extract_parse(item.backpointer[0]) + ")"
    else:  # Binary rule
        return "(" + item.symbol + " " + extract_parse(item.backpointer[0]) + " " + extract_parse(item.backpointer[1]) + ")"




def main():
    sentence = "the cat chased the dog with a stick".split()

    # Defining grammar
    grammar = [
        Rule("S", ["NP", "VP"], 1.0),
        Rule("NP", ["Det", "Noun"], 0.4),
        Rule("NP", ["Det", "NP"], 0.3),
        Rule("NP", ["Noun"], 0.3),
        Rule("VP", ["Verb", "NP"], 0.5),
        Rule("VP", ["Verb", "NP"], 0.3),
        Rule("VP", ["Verb"], 0.2),
        Rule("NP", ["NP", "PP"], 0.3),
        Rule("NP", ["Noun", "PP"], 0.3),
        Rule("PP", ["P", "NP"], 1.0),
        Rule("Det", ["the"], 0.6),
        Rule("Det", ["a"], 0.4),
        Rule("Noun", ["dog"], 0.2),
        Rule("Noun", ["cat"], 0.2),
        Rule("Noun", ["stick"], 0.2),
        Rule("Noun", ["chase"], 0.2),
        Rule("Noun", ["chased"], 0.2),
        Rule("Verb", ["chased"], 0.7),
        Rule("Verb", ["is"], 0.2),
        Rule("Verb", ["has"], 0.1),
        Rule("P", ["with"], 1.0)
    ]

    parses = nbest_CKY(sentence, grammar, 5)
    for prob, parse in parses:
        print(f"Probability: {prob}")
        print(f"Parse: {parse}")
        print()

if __name__ == "__main__":
    main()

