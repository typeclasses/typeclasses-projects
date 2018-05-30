{ pkgs, ... }:
{
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
  ec2.hvm = true;
  networking.hostName = "typeclasses-demo";
  environment.systemPackages = [ pkgs.vim pkgs.htop pkgs.emacs ];

  # Don't allow anyone to SSH using a password
  services.openssh.passwordAuthentication = false;

  # Let admins sudo without entering a password
  security.sudo.wheelNeedsPassword = false;

  # Let admins upload unsigned Nix packages
  nix.trustedUsers = ["@wheel"];

  users.users.chris = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];

    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDAu0nM8baJOgaBB44w/rCczNK7pST0b/B3isYtqYw8QI2nqkmp4zddAFTjqL/zyVgIC+I2iIINVdQrC6jRWWZc82VRBzHjUtcz3sPppkLvZvfVupSXli1bDtnKNLA73bGr+odZFyR2zWsgcPwpGkzaWotGVhsW/F/2J+6HwG/D+9pWTlFAPwK/OV2J3axdnWWcihXjMIWT5/5ksOPqQWUBQnrQDQt2Rtw+3Qg36UlAOQgj2SMmMg6ppf8k0A7WRVqGxJE0BrZwUfvfqmUidPOsitkUphOtM424+HyXv66bDdLr80apPCqrwllHv0tt4N6vWVQg5OeKQe2w4NvIjQlR1ROf3MC3/1itiGvo5saEUh0DtQrl/g9LGXmIkVHFzJsmfnIx74VXH9MESugHturxniB+8Rq7ICg4eWZAal46tgcMdOm7u313fm5HpumQRwqTWcghXWRmOopqDNzXT2CEOJ6gzJS50E9SI65Y501NTtXwBgQIyOSt5wp2edBdxZUIEj4yGBBaVwDREbuR0mEMNnEmWyd7DsyOzKnFCsb5fwm77S5uSISzbzy2boVyb/zBEFRAorYNIaWRPX+a9aN5Kki+xCIsfMhVBFxSc5PMVrn/a05pYg9eb0jJoWmeBCXVLxOitSX0UrpNMw9yd1Jrgv6wVe5wr5kiIad2ooxQvQ== cubby-1"
    ];
  };

  users.users.julie = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];

    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDUA5QiRaMriKQrvA1H74UsJcnGieAcHYrHp27KBZjOhhKvinTkNyErY5JNFbgrCh3oC+6HeMSUOp+5qb2/hF0aO6kdYbGnYB3BchrbsPYFDccqW9eQONXdvYsa4mavfLzHIvXktkxVsd71CBRXqVFxxolYps99iJvFfhmgnn9Iz+zUIuOSRCPQ/Bcxbh3+fZHD2wlPpOZ352V3/utR6n6nlhK5W1Gq/mp5dmE32zCEOEse85xXQNxJpjdhIDZbu8PnNo2LuYIfRBFzIj/Gh6MIHJU3gQskt8MvKA+POdseflPLznxoDWYT3FqBO+agObr3FGnlnwpi1g9ym+U8T6SNFGUwR7cX5VkegLNSl7FTLMtaXoqUTKhBQH+ZIpNwlyepYnFo1BHR3IgsFDSaD/zLUjesBQIw6j+mtDCK9P3EnUcXo5v04OuGoyqTtAF4TTAz2kuC8ZsCs0cQEZRIoXqVIRcvPmlFr208o0SeGakCHVxIj6VnrQ+aHisVwuGAkq+Kb5mwE6b0xvuFGHo+bqHIUePymWQihbh0pZpYeSaJTLb2YG17HmE2rUFeO66CADmIMi2EWEI0xisT/e9FoqDcMnG/Z5sLBBDkerXwyfLA63zAkF0P3LtWX1Q0vYFhCn+VtMwE56qW/Z0sDdNiVwxn/a1GGh12gqXkNHBEePYjoQ== doriangray"
    ];
  };
}
