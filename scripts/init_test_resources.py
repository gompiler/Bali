import os
import sys
import os.path as p
import subprocess
import shutil

project_dir = p.abspath(p.dirname(p.dirname(__file__)))

if p.basename(project_dir) != 'Bali':
    print("Project path {} is not Bali".format(project_dir))
    exit(1)


def files_dir(*paths):
    return p.abspath(p.join(project_dir, 'files', *paths))


_java_dir = files_dir('java')

java_files = [f for f in os.listdir(_java_dir) if
              f.endswith('.java')]

if not java_files:
    print("No java files found")
    exit(0)

_gen_dir = files_dir('gen')

if p.isdir(_gen_dir):
    shutil.rmtree(_gen_dir)


def gen_dir(*paths):
    path = p.abspath(p.join(_gen_dir, *paths))
    os.makedirs(path, exist_ok=True)
    return path


javac_dir = gen_dir('javac')
javap_dir = gen_dir('javap')
javap_v_dir = gen_dir('javap_v')
krakatau_dir = gen_dir('krakatau')

sys.path.append(p.abspath(p.join(project_dir, 'Krakatau')))

from Krakatau.assembler.disassembly import Disassembler
from Krakatau.classfileformat.reader import Reader
from Krakatau.classfileformat.classdata import ClassData
from io import StringIO

for f in java_files:
    name = f[:-5]


    def j_path(base_dir):
        return p.abspath(p.join(base_dir, name + ".j"))


    print("Generating {}".format(f))
    path = p.join(_java_dir, f)
    subprocess.call(['javac', path, '-d', javac_dir])
    javac_path = p.join(javac_dir, name + '.class')
    with open(j_path(javap_dir), 'w+') as p_file:
        subprocess.call(
            ['javap', '-p', '-c', javac_path],
            stdout=p_file)
    with open(j_path(javap_v_dir), 'w+') as p_file:
        subprocess.call(
            ['javap', '-p', '-c', '-v', javac_path],
            stdout=p_file)
    with open(javac_path, 'rb') as c_file:
        clsdata = ClassData(Reader(c_file.read()))
        output = StringIO()
        Disassembler(clsdata, output.write, roundtrip=False).disassemble()
        with open(j_path(krakatau_dir), 'w+') as p_file:
            p_file.write(output.getvalue())
