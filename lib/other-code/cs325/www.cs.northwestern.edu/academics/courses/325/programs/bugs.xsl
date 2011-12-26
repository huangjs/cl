<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="html" indent="yes"
  doctype-public="-//w3c//dtd html 4.0 transitional//en"/>

<xsl:template match="/">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="bug-fixes">
<html>
<head>
<title>Programming Bugs</title>
<link rel="stylesheet" type="text/css" href="bugs.css" />
</head>
<body>
<xsl:apply-templates select="bug-fix"/>
</body>
</html>
</xsl:template>

<xsl:template match="bug-fix">
<table class="bug-fix">

<tr>
<th colspan="2" class="title"><xsl:value-of select="title"/></th>
</tr>

<tr>
<th class="label">Language</th>
<td class="content"><xsl:value-of select="language"/></td>
</tr>

<tr>
<th class="label">Symptoms</th>
<td class="content"><xsl:value-of select="symptoms"/></td>
</tr>

<tr>
<th class="label">Example</th>
<td class="content"><pre><xsl:value-of select="example"/></pre></td>
</tr>

<tr>
<th class="label">Explanation</th>
<td class="content"><xsl:value-of select="explanation"/></td>
</tr>

<xsl:apply-templates select="repairs"/>

</table>
</xsl:template>

<xsl:template match="repairs">
<xsl:apply-templates select="repair"/>
</xsl:template>

<xsl:template match="repair">
<tr>
<th colspan="2" class="title">Repair</th>
</tr>

<tr>
<th class="label">Precondition</th>
<td class="content"><xsl:value-of select="precondition"/></td>
</tr>

<tr>
<th class="label">Action</th>
<td class="content"><xsl:value-of select="action"/></td>
</tr>

<tr>
<th class="label">Result</th>
<td class="content"><pre><xsl:value-of select="result"/></pre></td>
</tr>

</xsl:template>

</xsl:stylesheet>