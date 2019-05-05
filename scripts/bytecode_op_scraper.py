import re
from dataclasses import dataclass

from bs4 import BeautifulSoup
import requests

url = "https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings"


def get_contents() -> BeautifulSoup:
    tmp = "tmp/bytecode_page.html"
    parser = "html.parser"
    try:
        with open(tmp, "r") as f:
            return BeautifulSoup(f, parser)
    except IOError:
        print("Fetching from url")
        page = requests.get(url)
        if page.status_code != 200:
            print("Failed to retrieve {}, status code {}".format(url,
                                                                 page.status_code))
            exit(1)
        with open(tmp, "wb+") as f:
            f.write(page.content)
        return BeautifulSoup(page.content, "html.parser")


def underscore_to_pascal(name):
    return ''.join(part.capitalize() for part in name.split('_'))


ignored_ops = {'lookupswitch', 'tableswitch', 'wide', 'impdep1', 'impdep2'}

arg_map = {
    '': [],
    '1: index': ['IRIndex'],
    '2: indexbyte1, indexbyte2': ['IRIndexw'],
    '1: byte': ['Word8'],
    '2: branchbyte1, branchbyte2': ['IRLabel'],
    '4: branchbyte1, branchbyte2, branchbyte3, branchbyte4': ['IRLabelw'],
    '2: index, const': ['IRIndex', 'Word8'],
    '4: indexbyte1, indexbyte2, 0, 0': ['IRIndexw'],
    '4: indexbyte1, indexbyte2, count, 0': ['IRIndexw', 'Word8'],
    '3: indexbyte1, indexbyte2, dimensions': ['IRIndexw', 'Word8'],
    '1: atype': ['IRType'],
    '2: byte1, byte2': ['Word16']
}

arg_haskell_conv = {'Word8': 'b1 "byte"', 'Word16': 'b2 "2 bytes"',
                    'IRIndex': 'b1 "index"',
                    'IRType': "dparse'",
                    'IRIndexw': 'b2 "indexw"',
                    'IRLabel': 'b2 "label"', 'IRLabelw': 'b4 "labelw"'}


@dataclass
class Instruction:
    name: str
    haskell_name: str
    op_code: str
    signature: str
    args: [str]

    @staticmethod
    def create(el):
        tds = el.find_all('td')
        name = tds[0].text.strip()
        if name in ignored_ops:
            return None
        op_code = tds[1].text.strip()
        try:
            int(op_code, 16)
        except ValueError:
            return None
        return Instruction(name, op_code, tds)

    def __init__(self, name, op_code, tds):
        self.name = name
        self.haskell_name = underscore_to_pascal(name)
        self.op_code = op_code
        self.signature = tds[4].text.strip().replace('→', '->')
        arg_info = tds[3].text.strip()
        if arg_info not in arg_map:
            raise ValueError(
                'Unhandled arg info for {}: {}'.format(self.name, arg_info))
        self.args = arg_map[arg_info]

    def haskell_data(self):
        return '{} {} -- {}: {}'.format(self.haskell_name,
                                        ' '.join(self.args), self.name,
                                        self.signature,
                                        )

    def haskell_parser(self):

        if not self.args:
            builder = 'pure {}'.format(self.haskell_name)
        else:
            builder = self.haskell_name
            for i, a in enumerate(self.args):
                builder = builder + (' <$> ' if i == 0 else ' <*> ')
                if a in arg_haskell_conv:
                    builder = builder + arg_haskell_conv[a]
                else:
                    builder = builder + 'undefined {{- {} -}}'.format(a)
        return '0x{} -> {}'.format(self.op_code, builder)

    def haskell_map(self):
        if not self.args:
            builder = 'pure {}'.format(self.haskell_name)
        else:
            builder = self.haskell_name
            for i, a in enumerate(self.args):
                builder = builder + (' <$> ' if i == 0 else ' <*> ')
                builder = builder + a
        return '{} -> {}'.format(self.haskell_name, builder)

    def haskell_showj(self):
        s = '{} -> byteString "{}"'.format(self.haskell_name, self.name)
        if self.args:
            s = s + " -- todo; {} args".format(len(self.args))
        return s


soup = get_contents()

table = soup.find('table', class_="wikitable sortable")

if table is None:
    print("Could not find wiki table")
    exit(1)

instructions = sorted(filter(lambda x: x is not None,
                             map(Instruction.create,
                                 table.find_all('tr')[1:])),
                      key=lambda x: x.name)


def print_data():
    print('data Instruction')
    for i, instr in enumerate(instructions):
        if i == 0:
            print('  = ', end='')
        else:
            print('  | ', end='')
        print(instr.haskell_data())


def print_parser():
    for instr in instructions:
        print(instr.haskell_parser())


def print_showj():
    for instr in instructions:
        print(instr.haskell_showj())


def print_map():
    for instr in instructions:
        print(instr.haskell_map())


print_map()
