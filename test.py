import difflib
import glob
import os.path
import subprocess
import sys

def output_of(args):
    return subprocess.check_output(args).decode('ascii', 'ignore').split("\n")

testcases = []

test_path = "test"
if len(sys.argv) > 1:
    test_path = sys.argv[1]

if os.path.isdir(test_path):
    for root, dirs, files in os.walk(test_path):
        for file in files:
            if not file.endswith(".py"):
                continue

            testcase = os.path.join(root, file)
            testcases.append(testcase)
else:
    testcases.append(test_path)

testcases.sort()

errors = []
failures = []

for testcase in testcases:
    try:
        #print("Testing %s..." % testcase.replace("test/", ""))

        python_cmdline = ["python3", testcase]
        python_output = output_of(python_cmdline)

        hython_cmdline = ["./hython", testcase]
        hython_output = output_of(hython_cmdline)

        if python_output == hython_output:
            sys.stdout.write(".")
            sys.stdout.flush()
        else:
            sys.stdout.write("F")
            sys.stdout.flush()

            diff = difflib.unified_diff(python_output, hython_output, lineterm='', 
                    fromfile=" ".join(python_cmdline), tofile=" ".join(hython_cmdline))
            failures.append(diff)

    except subprocess.CalledProcessError as e:
        #print(dir(e))
        #print(e.stderr)
        #sys.exit(1)
        
        sys.stdout.write("E")
        sys.stdout.flush()

        errors.append(testcase)

print('')

if failures:
    print()
    print("Failures:")

for failure in failures:
    print("\n".join(failure))
    print()

if errors:
    print("Errors:")

for error in errors:
    print(error)

if errors or failures:
    sys.exit(1)

