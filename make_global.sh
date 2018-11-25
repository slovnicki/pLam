#!/bin/bash
################################################
# make the 'plam' command global in the system #
################################################

path_to_project=$(pwd)
echo "Your path to this project is $path_to_project"
echo "... adding the path to import/ into app/Config.hs"
echo "module Config where" > src/Config.hs
echo "" >> src/Config.hs
echo "importPath = \"$path_to_project/import/\"" >> src/Config.hs

path_to_bin=$(find .stack-work/install -name plam)
echo "Your path to plam binary is $path_to_bin"

plam_path="$path_to_project/$path_to_bin"

echo "... creating a script 'plam' in /usr/local/bin"
echo "#!/bin/bash" > /usr/local/bin/plam
echo "$plam_path \$@" >> /usr/local/bin/plam

echo "... making it executable"
chmod +x /usr/local/bin/plam

echo "Success!"


