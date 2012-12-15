var Essays = {

    addParagraphLinks : function() {
        jQuery("#content p").each(function(idx, el) {
            el.prepend("<a href='#'" + el.attr('id') + "' name='" + el.attr('id') + "'>&para;</a>")});
    }

};

jQuery(document).ready(function() {
    Essays.addParagraphLinks();
});
