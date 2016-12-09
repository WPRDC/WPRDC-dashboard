(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-62573248-4', 'auto');
ga('send', 'pageview');

//ga('send', 'event', 'category', 'action', 'label', value);

document.getElementById('downloadDatasetData').addEventListener("click", function () {
   ga('send', 'event', 'click', 'download', 'Download resource-stats table');
});
document.getElementById('downloadPackageData').addEventListener("click", function () {
   ga('send', 'event', 'click', 'download', 'Download package-stats table');
});
// Below is the HTML from which we have to fish out the links for the
// respective tabs:
//<ul class="nav nav-tabs shiny-tab-input" id="WPRDC-dashboard">
//          <li class="active">
//            <a href="#tab-5098-1" data-toggle="tab" data-value="Web stats">Web stats</a>[...]
$("a[data-value='Web stats']").addEventListener("click", function () {
   ga('send', 'event', 'click', 'switch to tab', 'View Web stats tab');
});

$("a[data-value='Other web stats']").addEventListener("click", function () {
   ga('send', 'event', 'click', 'switch to tab', 'View Other web stats tab');
});

$("a[data-value='Resource stats']").addEventListener("click", function () {
   ga('send', 'event', 'click', 'switch to tab', 'Resource stats tab');
});
//            <a href="#tab-5098-2" data-toggle="tab" data-value="Other web stats">Other web stats</a>[...]
//            <a href="#tab-5098-3" data-toggle="tab" data-value="Resource stats">Resource stats</a>[...]
//            <a href="#tab-5098-4" data-toggle="tab" data-value="Package stats">Package stats</a>[...]
//            <a href="#tab-5098-5" data-toggle="tab" data-value="Classroom uses">Classroom uses</a>[...]
$("a[data-value='Package stats']").addEventListener("click", function () {
   ga('send', 'event', 'click', 'switch to tab', 'Package stats tab');
});
$("a[data-value='Classroom uses']").addEventListener("click", function () {
   ga('send', 'event', 'click', 'switch to tab', 'Classroom uses tab');
});
$("a[data-value='Outreach']").addEventListener("click", function () {
   ga('send', 'event', 'click', 'switch to tab', 'Outreach tab');
});
//            <a href="#tab-5098-6" data-toggle="tab" data-value="Outreach">Outreach</a>[...]
//$("a[data-value='" + current +"']");
//http://stackoverflow.com/questions/4191386/jquery-how-to-find-an-element-based-on-a-data-attribute-value

