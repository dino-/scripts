# scripts


## Synopsis

Miscellaneous scripts (bash, Haskell, Perl)


## Description

This is a project for collecting those single-file scripts that
we all end up writing over time. You want to use version control,
but it's just one file. They stay in this project until they become
big enough to have a project of their own.


### Some of the more useful scripts in this repo

Much of this is specific to something I needed, but some of these may be of use
to you.

   * **any2webp.sh** - Convert JPG, PNG and GIF image files to WEBP with some
      default settings. Uses magick
   * **args-example.sh** - Example of args parsing with usage output
   * **burn-cd** - Perform the common-case CD burning I often need
   * **burn-dvd** - Perform the common-case DVD burning I often need
   * **cdcheck** - Thoroughly check the contents of a CD or DVD
      using md5sum
   * **clamwrap.sh** - Wrapper script to add some useful defaults to clamscan.
     Uses clamscan from clamav
   * **clrvimtmp** - List and optionally remove Vim swap files
   * **disp** - Change active video outputs using xrandr, for
      docking laptops
   * **dpi.hs** - Calculate DPI from screen resolution and screen
      size
   * **exifsetdate** - Set a file's Exif.Image.DateTime to a
      specific value
   * **finddate** - Recursively list all files with last modified
      datetime, sorted
   * **ghc-uninst** - 'clean' removal of globally-installed GHC
      packages
   * **hstestbuild** - Perform a test build on a Haskell package
      to ensure it's complete
   * **mdconv** - Convert Markdown to HTML or PDF with Pandoc
   * **megawho** - Check registration of domain names
   * **mkcarepkg** - Make a 'care package' of files from your home
      directory, for getting up to speed on a new system
   * **osx-clean.sh** - Blow away `__MACOSX` and `.DS_Store` items from a
      directory structure. These are often accidentally bundled with software
      made on Macs.
   * **rsync-errors.sh** - grep for any error reports in an rsync log file
   * **sendme** - Send yourself a simple email from the shell
   * **sshtun** - Establish a persistent SSH tunnel on a remote
      system
   * **withmount.hs** - Mount a filesystem, perform the supplied
      shell command, unmount it
   * **ziptest.sh** - Test a zip file for damage


## Getting source

    $ git clone https://github.com/dino-/scripts.git

Or [browse the source](https://github.com/dino-/scripts.git) at the same URL


## Development

Scripts being pulled out into their own projects need some things:

  - Author email address
  - Version number
  - Uniform and comprehensive help
  - More idiot-proofing with args
  - License


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
