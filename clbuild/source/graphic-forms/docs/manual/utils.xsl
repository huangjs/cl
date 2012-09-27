<?xml version="1.0"?>
<!--
    utils.xsl

    Copyright (c) 2007, Jack D. Unrue
-->
<xsl:stylesheet
  xmlns:exsl="http://exslt.org/common"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">

  <xsl:template name="create-id">
    <xsl:value-of select="concat(../@name,':',@name)"/>
  </xsl:template>

  <xsl:template name="upcase">
    <xsl:param name="orig-text"/>
    <xsl:value-of select="translate($orig-text,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
  </xsl:template>

  <xsl:template name="first-word">
    <xsl:param name="raw-text"/>
    <xsl:choose>
      <xsl:when test="contains($raw-text,' ')">
        <xsl:value-of select="substring-before($raw-text,' ')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$raw-text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
