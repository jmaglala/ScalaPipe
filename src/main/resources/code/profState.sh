#!/bin/bash
get_text() {
    o=$1
    size=`readelf -a $o | awk 'f{print;f=0} /] .text/{f=1}' | head -1 |cut -d " " -f8 | sed 's/0*\(.*\)/0x\1/g'`
    printf "%d\n" $size
}

stats=".profiles"
rm -f $stats
touch $stats

base_size=0
base="Segment.o Kernel.o Edge.o SPQ.o TSPQ.o"
for f in $base; do
    seg_size=$(get_text $f)
    base_size=$((base_size + seg_size))
done

printf "BASE:%d\n" $base_size >> $stats

cfiles=".c_file_list"
for f in `cat $cfiles`; do
    name=`echo $f | sed 's/\..*//'`
    name=$(basename $name)
    mod_size=$(get_text "$name/$name.o")
    printf "%s:%d\n" $name $mod_size >> $stats
done