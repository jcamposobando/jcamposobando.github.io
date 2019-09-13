---
layout: post
title: JRE Cryptographic Algorithms and SMTP security
tags: java security smtp email authentication 
---

I’m working on a project that requires sending an email to the client in every transaction. The email service runs fine in production, but when I run it on my computer it doesn’t work. The error message looks like this

```
javax.mail.MessagingException: Could not convert socket to TLS;
nested exception is:
javax.net.ssl.SSLHandshakeException: java.security.cert.CertificateException: Certificates do not conform to algorithm constraints
```

My first instinct was to check the `SMTP` server to confirm that it was working appropriately.

# Format your credentials with base64

According to Wikipedia:

> […] Base64 is a group of binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation. The term Base64 originates from a specific MIME content transfer encoding. Each Base64 digit represents exactly 6 bits of data.

There’s a handy little program to convert to base64 in Linux which is called, creatively enough, `base64`. To convert a file just do

```
base64 <fileName>
```
and to decode it

```
base64 -d <fileName>
```

if you git it no parameters, it expects something in the standard input, so, for converting our user and password we can do

```
$ echo -n “user” | base64
dXNlcg==
$ echo -n “password” | base64
cGFzc3dvcmQ=
```

The `-n`
 parameter is used to avoid the output of the trailing newline. We can 
decode those strings using base 64 to check that it worked

```
$ echo -n “dXNlcg==” | base64 -d
user
$ echo -n “cGFzc3dvcmQ=” | base64 -d
password
```

Now to the fun part.

# Logging in to the email server

First of all, I tried to log in to the `SMTP` server. I tried doing so by telnet:

## telnet

Type the following command in you terminal

```
telnet <yourEmailServerIPorDomainName> 25
```

Then, to start a connection with the `ESMTP` protocol use the command `EHLO` followed by your domain name. According to [D. J. Bernstein](https://cr.yp.to/smtp/ehlo.html)

> […] The client sends this command to the SMTP server to identify itself and initiate the SMTP conversation. […]

> […] EHLO is just like [HELO](https://cr.yp.to/smtp/helo.html#helo) except that the server’s response text provides computer-readable information about the server’s abilities. […]

```
EHLO <DomainName>
```

After that, you should see a list with the `SMTP` extensions supported by the server. The output should look a little like this:

```
250-mail.budget.com.ni  
250-PIPELINING  
250-SIZE 52428800  
250-VRFY  
250-ETRN  
250-STARTTLS
```

We are looking for `AUTH LOGIN`, which allows authentication of users. This server doesn’t have that enabled but has the `STARTSSL` extension enabled, so one can try to log in to the server using SSL.

## OpenSSL

It is possible to start an `SSL` connection with the server using telnet, but it can be cumbersome since the messages are encrypted. `OpenSSL` offers a simple solution for this. We will start a session using the command

```
openssl s_client -debug -starttls smtp -crlf -connect <domainName>:25
```

and then the `EHLO` command to get the enabled extensions

```
EHLO <domainName>
```

Now, the `AUTH LOGIN` extension IS enabled, so, to log we do

```
AUTH LOGIN
```

It should answer with

```
334 VXNlcm5hbWU6
```
where `VXNlcm5hbWU6` stands for “Username” in `base64`. Type your user name in `base64` and hit enter. Now the server answers with

``` 
334 UGFzc3dvcmQ6
```

which stands for “Password”. Type your password and you are in. From there everything is pretty easy. To send a mail type

```
mail from: <sender>@<domainName>.com
rcpt to: <recipientEmail>
subject: <Subject>
data: <body of the email>
```

To finish your email type `ENTER . ENTER` (that’s the enter key, then the dot key, then the enter key again)

Since sending an email from the terminal worked, the email server was working fine, so the error was in the client application.

## Java security

Turns out there is a configuration file that defines which algorithms for encryption aren’t safe anymore. You can read more about it [here](https://www.java.com/en/configure_crypto.html).

The problem arose because in newer versions of java the algorithm used for encryption by the server was no longer deemed safe by the `JRE`. There are several solutions.

## Changing the configuration file

In Fedora, the configuration file is in `/etc/crypto-policies/back-ends/java.config` and it looks like this

``` 
jdk.tls.ephemeralDHKeySize=1023  
jdk.certpath.disabledAlgorithms=MD2, MD5, DSA, RSA keySize < 2048  
jdk.tls.disabledAlgorithms=DH keySize < 1023, SSLv2, SSLv3, DHE_DSS, RSA_EXPORT, DHE_DSS_EXPORT, DHE_RSA_EXPORT, DH_DSS_EXPORT, DH_RSA_EXPORT, DH_anon, ECDH_anon, DH_RSA, DH_DSS, ECDH, 3DES_EDE_CBC, DES_CBC, RC4_40, RC4_128, DES40_CBC, RC2, HmacMD5  
jdk.tls.legacyAlgorithms=
```

if we wanted to allow, let’s say, `RSA 1024`, we would need to change the second line, after the less-than character. This approach doesn’t seem appropriate for me because it reduces the safety of the whole system, and affects any program running in a `JVM` in that machine.

## Using a command-line argument

It is possible to pass a file containing the security configurations on the command line.

```
java -Djava.security.properties= <fileName>
If you use Maven or any other building systems you may need to take a`itional considerations on where and how to a` that parameter.
```

## Changing it at runtime

You can change the variable adding this line to your program

``` java
java.security.Security.setProperty(“jdk.certpath.disabledAlgorithms”, “MD2, MD5, DSA, RSA keySize < 2048”);
```
