var Essays = {

    addParagraphLinks : function() {
        jQuery("#content p").each(function(idx, el) {
            el.innerHTML = "<a href='#" + el.id + "' name='" + el.id + "'>&para;</a>" + el.innerHTML;
        });
    }
};

jQuery(document).ready(function() {
    Essays.addParagraphLinks();
});
