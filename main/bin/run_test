#!/bin/sh

usage()
{
    >&2 echo "run_test [DIR] MODULE..."
    >&2 echo "    Evaluate <MODULE>:start/0 in each module in the current directory,"
    >&2 echo "    alternatively test modules as defined in <DIR>/active_tests.dat"
    exit 100
}

while :; do
    case $1 in
        --help | -h)
            usage
            ;;
        *)
            break
            ;;
    esac
    command shift
done

if [ "$#" -lt 1 ]; then
    usage
fi

bin_dir=${0%/*}
base_dir=${bin_dir}/../..
${bin_dir}/bespoke -- -noinput -run test_engine start ${base_dir} $*
