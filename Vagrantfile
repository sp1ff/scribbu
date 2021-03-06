# -*- mode: ruby -*-
# vi: set ft=ruby :

$provision = <<SCRIPT
echo "Provisioning..."
date > /etc/vagrant.provision_begin

set -e

head -n2 /etc/resolv.conf > /tmp/resolv.conf
echo nameserver 8.8.8.8 >> /tmp/resolv.conf
echo nameserver 8.8.4.4 >> /tmp/resolv.conf
tail -n -2 /etc/resolv.conf >> /tmp/resolv.conf
sudo mv -v /tmp/resolv.conf /etc/resolv.conf

sudo apt-get update

sudo apt-get -y install 'g++-5'
test -s /usr/bin/g++ || sudo ln -s /usr/bin/g++-5 /usr/bin/g++
sudo apt-get -y install doxygen
sudo apt-get -y install graphviz
sudo apt-get -y install gdb

# from source...
# sudo apt-get -y install libncurses5-dev
# wget ftp://sourceware.org/pub/gdb/releases/gdb-7.8.tar.gz
# tar xvf gdb-7.8.tar.gz 
# cd gdb-7.8/
# ./configure
# make
# sudo make install

sudo apt-get -y install build-essential devscripts
sudo apt-get -y install libbz2-dev
sudo apt-get -y install libunistring-dev
sudo apt-get -y install libffi-dev
sudo apt-get -y install python-dev
sudo apt-get -y install guile-2.0-dev
sudo apt-get -y install libboost-all-dev

# From source...
# wget http://sourceforge.net/projects/boost/files/boost/1.61.0/boost_1_61_0.tar.gz
# tar xvf boost_1_61_0.tar.gz
# cd boost_1_61_0
# ./bootstrap.sh
# sudo ./b2 --build-type=complete --layout=versioned install

wget ftp://ftp.gnu.org/gnu/guile/guile-2.2.0.tar.gz
tar -zxvf guile-2.2.0.tar.gz
cd guile-2.2.0/
./configure
make
sudo make install
sudo ldconfig

sudo apt-get -y install autoconf
sudo apt-get -y install automake
sudo apt-get -y install libtool
sudo apt-get -y install libtool-bin
sudo apt-get -y install texinfo
sudo apt-get -y install texlive texlive-latex-extra
sudo apt-get -y install libssl-dev
sudo apt-get -y install flex
sudo apt-get -y install bison
sudo apt-get -y install global ntp

echo source /vagrant/.vmbashrc >> /home/vagrant/.bashrc

ln -s /vagrant/.guile ~/.guile
ln -s /vagrant/.gdbinit ~/.gdbinit

echo "Provisioning...done."
date > /etc/vagrant.provision_end

SCRIPT

# All Vagrant configuration is done below. The "2" in
# Vagrant.configure configures the configuration version (we support
# older styles for backwards compatibility). Please don't change it
# unless you know what you're doing.
Vagrant.configure(2) do |config|

  # The most common configuration options are documented and commented
  # below.  For a complete reference, please see the online
  # documentation at https://docs.vagrantup.com.

  # Every Vagrant development environment requires a box. You can
  # search for boxes at https://atlas.hashicorp.com/search.
  config.vm.box = "ubuntu/xenial64"

  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs `vagrant
  # box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific
  # port within the machine from a port on the host machine. In the
  # example below, accessing "localhost:8080" will access port 80 on
  # the guest machine.
  config.vm.network "forwarded_port", guest: 8080, host: 18080

  # Create a private network, which allows host-only access to the
  # machine using a specific IP.
  # config.vm.network "private_network", ip: "192.168.1.1"

  # Create a public network, which generally matched to bridged
  # network.  Bridged networks make the machine appear as another
  # physical device on your network.
  config.vm.network "public_network", ip: "192.168.0.12", bridge: "eth0"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "/home/mgh/doc/hacking/mp3z", "/mp3z"
  # config.vm.synced_folder "/mnt/Took-Hall/mp3", "/mp3"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific
  # options.  Example for VirtualBox:

  config.vm.provider "virtualbox" do |vb|
    # Display the VirtualBox GUI when booting the machine
    vb.gui = true
  
    # Customize the amount of memory on the VM:
    vb.memory = "2048"
    vb.cpus = 2

    # http://serverfault.com/questions/409794/vagrant-virtualbox-cant-resolve-some-domains-from-w-in-vm
    vb.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
    # http://stackoverflow.com/questions/19490652/how-to-sync-time-on-host-wake-up-within-virtualbox
    vb.customize ["guestproperty", "set", :id, "/VirtualBox/GuestAdd/VBoxService/--timesync-set-threshold", 1000]
    # https://github.com/chef/bento/issues/682
    vb.customize ["modifyvm", :id, "--cableconnected1", "on"]
  end

  # View the documentation for the provider you are using for more
  # information on available options.

  # Define a Vagrant Push strategy for pushing to Atlas. Other push
  # strategies such as FTP and Heroku are also available. See the
  # documentation at https://docs.vagrantup.com/v2/push/atlas.html for
  # more information.
  # config.push.define "atlas" do |push| 
  #   push.app ="YOUR_ATLAS_USERNAME/YOUR_APPLICATION_NAME"
  # end

  # Enable provisioning with a shell script. Additional provisioners
  # such as Puppet, Chef, Ansible, Salt, and Docker are also
  # available. Please see the documentation for more information about
  # their specific syntax and use.
  config.vm.provision "shell", inline: $provision
  # config.vm.provision :shell, :inline => "sudo rm /etc/localtime && sudo ln -s /usr/share/zoneinfo/US/Pacific /etc/localtime", run: "always"

end
