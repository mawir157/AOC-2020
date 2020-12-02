echo "			 --- Running Haskell code ---"
cd Haskell
./run.sh $1
cd ..
echo

echo "			 --- Running Go code ---"
cd Go
./run.sh $1
cd ..

echo
echo "			 --- Running Rust code ---"
cd Rust
./run.sh $1
cd ..