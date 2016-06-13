Debugging
=========

/vagrant/configure CXXFLAGS='-g -O0'
make
libtool --mode=execute gdb --args src/scribbu list /vagrant/test/data
