var consortUIBinding = new Shiny.InputBinding();

$.extend(consortUIBinding, {

  find: function(scope) {
    return $(scope).find(".consort");
  },

  initialize: function(el) {
    var onlyone = $(el).hasClass("one-to-one");

    if(onlyone) {
      $("#" + el.id).sortable({
        connectWith: ".consort",
        receive:  function(event, ui) {
        if($(event.target).children().length > 1) {
        $(ui.sender).sortable("cancel");
        } else {
         $(ui.item).removeClass("un-used").addClass("used");
        }
  	   }
      });
    } else {
      $("#" + el.id).sortable({
        connectWith: ".consort",
        receive: function(event, ui) {
          $(ui.item).removeClass("used").addClass("un-used");
        }
      });
    }
  },

  getId: function(el) {
    return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
  },

  getValue: function(el) {
    return $(el).children().map(function(i, e){
        return $(e).attr("data-value");
    }).get();
  },

  subscribe: function(el, callback) {
    $(el).on('sortable.consortUIBinding', function(event) {
      callback(false);
    });

    $(el).on('sortupdate.consortUIBinding', function(event) {
      callback(false);
    });
  },

  unsubscribe: function(el) {
    $(el).off('.consortUIBinding');
  }

});

Shiny.inputBindings.register(consortUIBinding, "dive.consortUI");
