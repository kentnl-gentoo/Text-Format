# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..1\n"; }
END {print "not ok 1\n" unless $loaded;}
use Text::Format 0.32;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

$text = new Text::Format;
@text = $text->paragraphs("hello world","cool");
print "not ok 2\n" unless @text == 2;

@text = $text->format("hello world","cool");
print "not ok 3\n" unless @text == 1;

@text = $text->center("hello world","cool");
print "not ok 4\n" unless @text == 2;

$text->columns(10);
$text->bodyIndent(8);
@text = $text->format("hello world","cool");
print "not ok 5\n" unless @text == 3;
