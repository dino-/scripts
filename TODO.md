### clrvimtmp

- Rewrite in bash instead of Perl, I bet it'll be smaller even
  use this:

      for F in $(find $HOME -regex '.*sw[mnop]')
      do
         stat --printf="%n\n   %y\n" $F
      done


### epochconv.hs

- Better error message when this fails: 2014-08-25


### exifsetdate

- Document this thing so it's clear how to use it


### megawho

- Add info in usage: Please don't be a jerk with mega hits to
  whois services.


### mkcarepkg

- Need some usage info


### tidy-ind, tidy-xml

- Found these switches in a file called tidy-html.conf, figure out
  how to bring all this together into one script perhaps

   indent-spaces: 3
   indent-attributes: yes
   wrap: 0


### withmount.hs

- This is becoming a part of the "davbox" project and needs to be
  a real binary-producing project that can be installed with distro
  packaging.
- Deal with umount/fusermount confusion. Maybe try umount first
  and check for failure.
- Would like a verbose option. For this to happen, needs to have
  more serious arg parsing and probably be a project of its own.


### general notes

- In a perfect world, all scripts would check for existence of
  binaries they call
- Change all scripts to use here document for usage notes


### scripts going public

Candidates for this:

- megawho
- withmount.hs


Scripts slated for public release need some things:

- Author email address
- Version number
- Uniform and comprehensive help
- More idiot-proofing with args
- Licensing
