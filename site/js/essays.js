var Essays = {

    addParagraphLinks : function() {
        jQuery("#content p").each(function(idx, el) {
            var link = document.createElement("a");
            link.setAttribute("class", "paragraph-link");
            link.setAttribute("name", el.id);
            link.setAttribute("href", "#" + el.id);
            link.innerHTML = "&para;";

            el.insertBefore(link, el.firstChild);
            el.addEventListener("mouseover", function() { link.style.color = "#AAAAAA"; }, false);
            el.addEventListener("mouseout", function() { link.style.color = "#FFFFFF"; }, false);
        });
    }
};

jQuery(document).ready(function() {
    Essays.addParagraphLinks();
});
