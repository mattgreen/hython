import difflib
import glob
import os.path
import subprocess
import sys

def output_of(args):
    return subprocess.check_output(args).decode('ascii', 'ignore').split("\n")

exit_status = 0

for testcase in glob.glob("test/*.py"):
    print("Testing %s..." % os.path.basename(testcase))

    python_cmdline = ["python3", testcase]
    python_output = output_of(python_cmdline)

    hython_cmdline = ["./hython", testcase]
    hython_output = output_of(hython_cmdline)

    if python_output != hython_output:
        diff = difflib.unified_diff(python_output, hython_output, lineterm='', 
                fromfile=" ".join(python_cmdline), tofile=" ".join(hython_cmdline))
        print("\n".join(diff))

        exit_status = 1

if exit_status == 0:
    print("All test cases passed!")

sys.exit(exit_status)
