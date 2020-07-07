// some javascript utility functions used in app
Shiny.addCustomMessageHandler("resetFileInput",
  function(message) {
    var id = message;
    $("#" + id).val('');
    $("#" + id + "_progress").css("visibility", "hidden");
    $("#" + id + "_progress").find(".progress-bar").css("width", "0");
    $("#" + id).closest(".input-group").find("input[type='text']").val('');
});
