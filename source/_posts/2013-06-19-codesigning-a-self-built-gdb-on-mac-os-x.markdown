---
layout: post
title: "Codesigning a self-built GDB on Mac OS X."
date: 2013-06-19 18:20
comments: true
published: false
categories: 
---

Steps adapted from the GNU [GNAT manual][1], which I found linked via [Stack Overflow][2].  I was actually concerned about whether I could legimately
quote the manual (which is licensed under the [GNU FDL][3], [version 1.3][4]).  But based on the discussion on the emacs-devel list [5], it looks to me like the people will see that this is a good faith effort to share knowledge, and not to subvert the GNAT project or manual itself.  Furthermore, the fact that I am linking to the FDL in this post might well be satisfying the requirements of the license, in spirit if not in word.

<!-- more -->

Anyway, here are the steps.

1. Open `Keychain Access.app`
2. Select menu item
   "Keychain Access" -> "Certificate Assistant" -> "Create a Certificate".
   Don't worry about what context you are in in the application itself,
   this is a global operation.

   * Choose a name for the new certificate; the choice "gdb-cert" is
     used in the GNU GNAT manual, and I chose it too.
   * Set "Identity Type" to "Self Signed Root"
   * Set "Certificate Type" to "Code Signing"
   * Activate the "Let me override defaults" option

3. Click on the "Continue" button repeatedly untile the
   "Specify a Location For the Certificate" pane appears; then
   set "Keychain" to "System"

4. Click on the "Continue" button until the certificate
   is created.  (For me, I think this was the final window
   in the series.)

5. In the main window, find the new certificate (putting "gdb-cert"
   into the search box may be of use for this), then double-click the
   new certificate, and set "When using this certificate" to "Always Trust".

6. Exit `Keychain Access.app` and *restart* the computer.  (Obligatory "Argh.")

----

Once the certificate is created, you codesign the debugger as follows:
`% codesign -f -s "gdb-cert" <gdb-install-path>/bin/gdb`

(You can also codesign the local build and then copy it elsewhere, if
you want to add this to your build process; but note that the
code-signing action prompts for a password to access the certificate.)

[1]: http://gcc.gnu.org/onlinedocs/gnat_ugn_unw.pdf

[2]: http://stackoverflow.com/questions/13913818/how-to-get-a-codesigned-gdb-on-osx

[3]: http://www.gnu.org/copyleft/fdl.html

[4]: http://www.gnu.org/licenses/fdl-1.3.txt

[5]: https://lists.gnu.org/archive/html/emacs-devel/2010-12/msg00540.html
