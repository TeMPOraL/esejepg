var Essays = {

    addParagraphLinks : function() {
        jQuery("#content p").each(function(idx, el) {
            el.prepend("<a href='#'" + el.id + "' name='" + el.id + "'>&para;</a>")});
    }

};

jQuery(document).ready(function() {
    Essays.addParagraphLinks();
});
