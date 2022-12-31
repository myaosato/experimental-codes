<?php

function withOutputToString(?string &$str, callable $proc)
{
    $stream = fopen('php://memory', 'w+');
    $ret = $proc($stream);
    $stack = '';
    rewind($stream);
    while (false !== ($c = fgetc($stream))) {
        $stack .= $c;
    }
    if ($str === null) {
        return $stack;
    }
    $str = $stack;
    fclose($stream);
    return $ret;
}

$str1 = '';
$ret1 = withOutputToString($str1, function($s) {
    fprintf($s, '%04d-%02d-%02d', 2022, 12, 31);
    return 42;
});
echo "str1: $str1\n";
echo "ret1: $ret1\n";

$str2 = null;
$ret2 = withOutputToString($str2, function($s) {
    fprintf($s, '%04d-%02d-%02d', 2022, 12, 31);
    return 42;
});
echo "str2: $str2\n";
echo "ret2: $ret2\n";

