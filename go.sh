g++ -std=c++14 main.cpp $(llvm-config --cxxflags) $(llvm-config --ldflags) -lLLVM $(llvm-config --system-libs) -fexceptions -o xbase
./xbase
