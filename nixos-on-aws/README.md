# Deploying NixOS to Amazon EC2

https://typeclasses.com/nixos-on-aws

Most of the code that powers the Type Classes website is written in Haskell and running on Amazon EC2. In this project we walk through the process of how we developed our deploy process. We start by clicking around in the AWS web console, and we end up with some scripts and a fairly simple process that we now use to provision our servers from the command line.

---

This directory contains:

  - The NixOS configuration file we created in the project video (`configuration.nix`)
  - The build and deploy scripts (`build.hs` and `deploy.hs`), with minor revisions
