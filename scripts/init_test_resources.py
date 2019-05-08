import os
import os.path as p
import subprocess
import shutil

project_dir = p.abspath(p.dirname(p.dirname(__file__)))

if p.basename(project_dir) != 'Bali':
    print("Project path {} is not Bali".format(project_dir))
    exit(1)


def files_dir(*paths):
    return p.abspath(p.join(project_dir, 'files', *paths))


java_dir = files_dir('java')

gen_dir = files_dir('gen')

java_files = [f for f in os.listdir(java_dir) if
              f.endswith('.java')]

if not java_files:
    print("No java files found")
    exit(0)

if p.isdir(gen_dir):
    shutil.rmtree(gen_dir)

os.makedirs(gen_dir, exist_ok=True)

for f in java_files:
    name = f[:-5]
    path = p.join(java_dir, f)
    subprocess.call(['javac', path, '-d', gen_dir])
    print(name, path, f)
