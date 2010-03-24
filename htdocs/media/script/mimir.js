$(document).ready(function() {
    var threshold = 10;

    $('ol').filter(function(index) {
        return $('li', this).length >= threshold;
    }).after('<div class="more"><a href="#">Show <span>less</span></a></div>');

    $('.more a').click(function(event) {
        event.preventDefault();
        var span = $('span', this);
        var items = $(this).parent().prev().children();
        if (span.text() == 'more') {
            items = items.filter(':hidden');
            items.addClass('new-additions');
            var t = setTimeout(function() {
                span.data('new-additions-reset', null);
                items.removeClass('new-additions');
            }, 2000);
            span.data('new-additions-reset', t);
            items.show();
            span.text('less');
        } else {
            items.slice(threshold).hide();
            var t = span.data('new-additions-reset');
            if (t) {
                span.data('new-additions-reset', null);
                clearTimeout(t);
            }
            span.text('more');
        }
    }).click();

    $('.comments a').hover(function(event) {
        $(this).parents('li').addClass('attention');
    }, function(event) {
        $(this).parents('li').removeClass('attention');
    }).click(function(event) {
        setTimeout(function() {
            $('.attention').removeClass('attention');
        }, 1000);
    });
});

