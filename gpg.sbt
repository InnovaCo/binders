pgpSecretRing := file("inn-oss-private.asc")

pgpPublicRing := file("inn-oss-public.asc")

usePgpKeyHex("5DF2525FA9D102B7")

pgpPassphrase := Option(System.getenv().get("oss_gpg_passphrase").toCharArray)
