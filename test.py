import difflib
import glob
import os.path
import subprocess
import sys

def output_of(args):
    return subprocess.check_output(args).decode('ascii', 'ignore').split("\n")

exit_status = 0

testcases = []
for root, dirs, files in os.walk("test"):
    for file in files:
        if not file.endswith(".py"):
            continue

        testcase = os.path.join(root, file)
        testcases.append(testcase)

testcases.sort()

for testcase in testcases:
    print("Testing %s..." % testcase.replace("test/", ""))

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
