# scripts


## Synopsis

Miscellaneous scripts (bash, Haskell, Perl)


## Description

This is a project for collecting those single-file scripts that
we all end up writing over time. You want to use version control,
but it's just one file. They stay in this project until they become
big enough to have a project of their own.


### Some of the more useful scripts in this repo

Much of this is specific to something I needed, but some of these may be of use to you.

   * **burn-cd** - Perform the common-case CD burning I often need
   * **burn-dvd** - Perform the common-case DVD burning I often need
   * **cdcheck** - Thoroughly check the contents of a CD or DVD
      using md5sum
   * **clrvimtmp** - List and optionally remove Vim swap files
   * **diceware** - Generate passphrases using the Diceware method
   * **disp** - Change active video outputs using xrandr, for
      docking laptops
   * **dpi.hs** - Calculate DPI from screen resolution and screen
      size
   * **epochconv.hs** - Show a given date (or the current date)
      in a variety of formats
   * **exifsetdate** - Set a file's Exif.Image.DateTime to a
      specific value
   * **finddate** - Recursively list all files with last modified
      datetime, sorted
   * **ghc-uninst** - 'clean' removal of globally-installed GHC
      packages
   * **hstestbuild** - Perform a test build on a Haskell package
      to ensure it's complete
   * **kickmutt** - Kill unresponsive mutt and blow away the
      temp files
   * **megawho** - Check registration of domain names
   * **mkcarepkg** - Make a 'care package' of files from your home
      directory, for getting up to speed on a new system
   * **mountiso** - Mount an iso image to /mnt
   * **new-hs.hs** - Create a very simple Haskell starter shell
      script
   * **new-pl** - Create a very simple Perl starter script
   * **sendme** - Send yourself a simple email from the shell
   * **sshtun** - Establish a persistent SSH tunnel on a remote
      system
   * **withmount.hs** - Mount a filesystem, perform the supplied
      shell command, unmount it
   * **ziptest.sh** - Test a zip file for damage


## Getting source

Get the source with darcs:

    $ darcs get http://ui3.info/darcs/scripts

Or [browse the source](http://ui3.info/darcs/scripts)


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
